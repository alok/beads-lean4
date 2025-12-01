/-
Beads JSONL Storage Implementation
File-backed storage using JSONL format for git compatibility
-/
import Beads.Storage.Storage
import Beads.Core.Id
import Lean.Data.Json

namespace Beads

open Lean Json

/-- JSONL Storage state -/
structure JsonlStorage where
  /-- Path to .beads directory -/
  beadsDir : System.FilePath
  /-- In-memory issue store -/
  issues : IO.Ref (Std.HashMap IssueId Issue)
  /-- In-memory dependency store -/
  dependencies : IO.Ref (List Dependency)
  /-- ID generator state -/
  idGenState : IO.Ref IdGenerator.State
  /-- Dirty flag for save optimization -/
  dirty : IO.Ref Bool

namespace JsonlStorage

/-- Path to issues.jsonl -/
def issuesPath (storage : JsonlStorage) : System.FilePath :=
  storage.beadsDir / "issues.jsonl"

/-- Path to deletions.jsonl -/
def deletionsPath (storage : JsonlStorage) : System.FilePath :=
  storage.beadsDir / "deletions.jsonl"

/-- Create a new JSONL storage at the given path -/
def create (beadsDir : System.FilePath) : IO JsonlStorage := do
  -- Create .beads directory if it doesn't exist
  IO.FS.createDirAll beadsDir
  let issues ← IO.mkRef {}
  let dependencies ← IO.mkRef []
  let idGenState ← IO.mkRef {}
  let dirty ← IO.mkRef false
  pure { beadsDir, issues, dependencies, idGenState, dirty }

/-- Parse a JSONL file into a list of JSON values -/
def parseJsonl (content : String) : List (Except String Json) :=
  content.splitOn "\n"
    |>.filter (fun s => !s.trim.isEmpty)
    |>.map Json.parse

/-- Load issues from JSONL file -/
def loadIssues (storage : JsonlStorage) : IO Unit := do
  let path := storage.issuesPath
  if ← path.pathExists then
    let content ← IO.FS.readFile path
    let lines := parseJsonl content
    let mut issueMap : Std.HashMap IssueId Issue := {}
    let mut deps : List Dependency := []
    let mut existingIds : List String := []

    for lineResult in lines do
      match lineResult with
      | .ok json =>
        match fromJson? json with
        | .ok (issue : Issue) =>
          issueMap := issueMap.insert issue.id issue
          existingIds := issue.id.value :: existingIds
          -- Extract embedded dependencies
          match json.getObjVal? "dependencies" with
          | .ok depArrJson =>
            match depArrJson.getArr? with
            | .ok depArr =>
              for depJson in depArr do
                match fromJson? depJson with
                | .ok (dep : Dependency) => deps := dep :: deps
                | .error _ => pure ()
            | .error _ => pure ()
          | .error _ => pure ()
        | .error e => IO.eprintln s!"Warning: Failed to parse issue: {e}"
      | .error e => IO.eprintln s!"Warning: Failed to parse JSON: {e}"

    storage.issues.set issueMap
    storage.dependencies.set deps
    storage.idGenState.modify fun s => { s with existingIds }
  pure ()

/-- Helper: lookup with default in HashMap -/
def hashMapFindD {α β : Type} [BEq α] [Hashable α] (m : Std.HashMap α β) (k : α) (d : β) : β :=
  m.get? k |>.getD d

/-- Helper: check if haystack contains needle as substring -/
def containsSubstr (haystack needle : String) : Bool :=
  needle.isEmpty || (haystack.splitOn needle).length > 1

/-- Save issues to JSONL file -/
def saveIssues (storage : JsonlStorage) : IO Unit := do
  let issues ← storage.issues.get
  let deps ← storage.dependencies.get

  -- Group dependencies by issue
  let depsByIssue : Std.HashMap IssueId (List Dependency) :=
    deps.foldl (fun m d =>
      let existing := hashMapFindD m d.issueId []
      m.insert d.issueId (d :: existing)) {}

  -- Build JSONL content
  let mut lines : List String := []
  for (_, issue) in issues.toList do
    let issueDeps := hashMapFindD depsByIssue issue.id []
    -- Add dependencies to issue JSON
    let issueJson := toJson issue
    let finalJson := match issueJson with
      | .obj fields =>
        if issueDeps.isEmpty then issueJson
        else
          let depsArr := Json.arr (issueDeps.map toJson).toArray
          .obj (fields.insert "dependencies" depsArr)
      | _ => issueJson
    lines := finalJson.compress :: lines

  let content := "\n".intercalate lines.reverse
  IO.FS.writeFile (storage.issuesPath) (content ++ "\n")
  storage.dirty.set false

/-- Check if adding a dependency would create a cycle (BFS from target to source) -/
def wouldCreateCycleImpl (storage : JsonlStorage) (from_ to : IssueId) : IO Bool := do
  let deps ← storage.dependencies.get
  -- BFS from 'to' to see if we can reach 'from_'
  -- Use partial since we have fuel-based termination
  pure (bfsLoop deps from_ [to] {} 1000)
where
  bfsLoop (deps : List Dependency) (target : IssueId) (queue : List IssueId)
      (visited : Std.HashSet IssueId) (fuel : Nat) : Bool :=
    if fuel == 0 then false  -- Give up if too many iterations
    else match queue with
    | [] => false
    | current :: rest =>
      if current == target then true
      else if visited.contains current then bfsLoop deps target rest visited (fuel - 1)
      else
        let neighbors := deps.filter (·.issueId == current) |>.map (·.dependsOnId)
        bfsLoop deps target (rest ++ neighbors) (visited.insert current) (fuel - 1)
  termination_by fuel
  decreasing_by all_goals simp_all; omega

/-- Compute the set of blocked issue IDs -/
def computeBlockedSetImpl (issues : List (IssueId × Issue)) (deps : List Dependency) : Std.HashSet IssueId :=
  let issueMap : Std.HashMap IssueId Issue := issues.foldl (fun m (id, i) => m.insert id i) {}
  -- Find directly blocked issues
  let directlyBlocked := deps.filter fun dep =>
    dep.depType.affectsBlocking &&
    match issueMap.get? dep.dependsOnId with
    | some issue => issue.status != .closed
    | none => false
  -- Build blocked set
  let blocked := directlyBlocked.foldl (fun s dep => s.insert dep.issueId) {}
  -- Propagate to children (simplified - just one level for now)
  let childDeps := deps.filter (·.depType == .parentChild)
  childDeps.foldl (fun s dep =>
    if s.contains dep.dependsOnId then s.insert dep.issueId else s) blocked

/-- Create storage operations from JsonlStorage -/
def toStorageOps (storage : JsonlStorage) : StorageOps := {
  createIssue := fun issue _actor => do
    let idState ← storage.idGenState.get
    let (newId, newIdState) := IdGenerator.generateId idState issue.title issue.description issue.createdAt
    storage.idGenState.set newIdState
    let newIssue := { issue with id := newId }
    storage.issues.modify fun m => m.insert newId newIssue
    storage.dirty.set true
    pure newId

  getIssue := fun id => do
    let issues ← storage.issues.get
    pure (issues.get? id)

  updateIssue := fun id f _actor => do
    storage.issues.modify fun m =>
      match m.get? id with
      | some issue => m.insert id (f issue)
      | none => m
    storage.dirty.set true

  deleteIssue := fun id => do
    storage.issues.modify fun m => m.erase id
    storage.dependencies.modify fun deps =>
      deps.filter fun d => d.issueId != id && d.dependsOnId != id
    storage.dirty.set true

  getAllIssues := do
    let issues ← storage.issues.get
    pure (issues.toList.map Prod.snd)

  searchIssues := fun filter => do
    let issues ← storage.issues.get
    let allIssues := issues.toList.map Prod.snd
    let filtered := allIssues.filter fun issue =>
      (match filter.status with | some s => issue.status == s | none => true) &&
      (match filter.priority with | some p => issue.priority == p | none => true) &&
      (match filter.issueType with | some t => issue.issueType == t | none => true) &&
      (match filter.assignee with | some a => issue.assignee == some a | none => true) &&
      (if filter.titleSearch.isEmpty then true else containsSubstr issue.title filter.titleSearch) &&
      (if filter.ids.isEmpty then true else filter.ids.contains issue.id)
    pure (filtered.take filter.limit)

  addDependency := fun dep _actor => do
    -- Check for cycles first
    let wouldCycle ← wouldCreateCycleImpl storage dep.issueId dep.dependsOnId
    if wouldCycle then
      pure (.error (.wouldCreateCycle dep.issueId dep.dependsOnId []))
    else
      storage.dependencies.modify fun deps => dep :: deps
      storage.dirty.set true
      pure (.ok ())

  removeDependency := fun issueId dependsOnId _actor => do
    storage.dependencies.modify fun deps =>
      deps.filter fun d => d.issueId != issueId || d.dependsOnId != dependsOnId
    storage.dirty.set true

  getDependencies := fun issueId => do
    let deps ← storage.dependencies.get
    pure (deps.filter fun d => d.issueId == issueId)

  getDependents := fun issueId => do
    let deps ← storage.dependencies.get
    pure (deps.filter fun d => d.dependsOnId == issueId)

  getAllDependencies := storage.dependencies.get

  addLabel := fun issueId label _actor => do
    storage.issues.modify fun m =>
      match m.get? issueId with
      | some issue =>
        if issue.labels.contains label then m
        else m.insert issueId { issue with labels := label :: issue.labels }
      | none => m
    storage.dirty.set true

  removeLabel := fun issueId label _actor => do
    storage.issues.modify fun m =>
      match m.get? issueId with
      | some issue => m.insert issueId { issue with labels := issue.labels.filter (· != label) }
      | none => m
    storage.dirty.set true

  getLabels := fun issueId => do
    let issues ← storage.issues.get
    pure (issues.get? issueId |>.map (·.labels) |>.getD [])

  getReadyWork := fun filter => do
    let issues ← storage.issues.get
    let deps ← storage.dependencies.get
    let blockedSet := computeBlockedSetImpl issues.toList deps
    let allIssues := issues.toList.map Prod.snd
    let ready := allIssues.filter fun issue =>
      issue.status.isOpenForWork &&
      !blockedSet.contains issue.id &&
      (match filter.status with | some s => issue.status == s | none => true) &&
      (match filter.priority with | some p => issue.priority == p | none => true) &&
      (match filter.assignee with | some a => issue.assignee == some a | none => true) &&
      (if filter.unassigned then issue.assignee.isNone else true)
    pure (ready.take filter.limit)

  getBlockedIssues := do
    let issues ← storage.issues.get
    let deps ← storage.dependencies.get
    let allIssues := issues.toList.map Prod.snd
    let blockedSet := computeBlockedSetImpl issues.toList deps

    let blockedIssues := allIssues.filterMap fun issue =>
      if !blockedSet.contains issue.id || !issue.status.isOpenForWork then none
      else
        let blockers := deps.filter fun d =>
          d.issueId == issue.id &&
          d.depType.affectsBlocking &&
          match issues.get? d.dependsOnId with
          | some depIssue => depIssue.status != .closed
          | none => false
        some {
          issue := issue
          blockedByCount := blockers.length
          blockedBy := blockers.map (·.dependsOnId)
        }
    pure blockedIssues

  wouldCreateCycle := wouldCreateCycleImpl storage

  detectCycles := do
    -- For now, return empty (we prevent cycles on add)
    pure []

  save := saveIssues storage

  load := loadIssues storage
}

/-- Open or create JSONL storage in current directory -/
def openOrCreate (path : System.FilePath := ".beads") : IO JsonlStorage := do
  let storage ← create path
  storage.loadIssues
  pure storage

end JsonlStorage

end Beads
