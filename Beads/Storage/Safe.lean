/-
Safe Storage Operations with Enhanced Error Handling
Wraps storage operations with validation and better error types
-/
import Beads.Storage.Storage
import Beads.Core.Validated

namespace Beads

/-- Detailed error type for storage operations -/
inductive StorageError where
  | issueNotFound (id : IssueId)
  | invalidTitle (reason : String)
  | invalidLabel (reason : String)
  | duplicateLabel (issueId : IssueId) (label : String)
  | labelNotFound (issueId : IssueId) (label : String)
  | cycleDetected (from_ to : IssueId) (path : List IssueId)
  | dependencyNotFound (issueId dependsOnId : IssueId)
  | selfDependency (issueId : IssueId)
  | persistenceError (reason : String)
  | validationError (field : String) (reason : String)
  deriving Repr

namespace StorageError

def toString : StorageError → String
  | .issueNotFound id => s!"Issue not found: {id}"
  | .invalidTitle reason => s!"Invalid title: {reason}"
  | .invalidLabel reason => s!"Invalid label: {reason}"
  | .duplicateLabel id label => s!"Label '{label}' already exists on issue {id}"
  | .labelNotFound id label => s!"Label '{label}' not found on issue {id}"
  | .cycleDetected from_ to _ => s!"Adding dependency from {from_} to {to} would create a cycle"
  | .dependencyNotFound issueId depId => s!"Dependency not found: {issueId} -> {depId}"
  | .selfDependency id => s!"Cannot add self-dependency on issue {id}"
  | .persistenceError reason => s!"Persistence error: {reason}"
  | .validationError field reason => s!"Validation error in {field}: {reason}"

instance : ToString StorageError := ⟨toString⟩

end StorageError

/-- Result type for safe storage operations -/
abbrev SafeResult (α : Type) := Except StorageError α

/-- Safe storage wrapper with validation -/
structure SafeStorage where
  ops : StorageOps

namespace SafeStorage

/-- Validate a title before creating/updating -/
def validateTitle (title : String) : SafeResult Title :=
  match Title.ofString? title with
  | some t => .ok t
  | none =>
    if title.isEmpty then .error (.invalidTitle "Title cannot be empty")
    else if title.length > 500 then .error (.invalidTitle "Title must be 500 characters or less")
    else .error (.invalidTitle "Unknown validation error")

/-- Validate a label before adding -/
def validateLabel (label : String) : SafeResult ValidLabel :=
  match ValidLabel.ofString? label with
  | some l => .ok l
  | none => .error (.invalidLabel "Label cannot be empty or whitespace-only")

/-- Get an issue or return error -/
def getIssue (s : SafeStorage) (id : IssueId) : IO (SafeResult Issue) := do
  match ← s.ops.getIssue id with
  | some issue => pure (.ok issue)
  | none => pure (.error (.issueNotFound id))

/-- Create an issue with validation -/
def createIssue (s : SafeStorage) (title description : String) (actor : String) : IO (SafeResult IssueId) := do
  match validateTitle title with
  | .error e => pure (.error e)
  | .ok validTitle =>
    let issue : Issue := {
      id := ⟨""⟩  -- Will be assigned
      title := validTitle.val
      description := description
      createdAt := ← IO.monoMsNow
      updatedAt := ← IO.monoMsNow
    }
    let id ← s.ops.createIssue issue actor
    pure (.ok id)

/-- Update an issue with validation -/
def updateIssue (s : SafeStorage) (id : IssueId) (f : Issue → Issue) (actor : String) : IO (SafeResult Unit) := do
  match ← s.ops.getIssue id with
  | none => pure (.error (.issueNotFound id))
  | some issue =>
    let updated := f issue
    -- Validate the update
    if updated.title.isEmpty then
      pure (.error (.validationError "title" "Title cannot be empty"))
    else if updated.title.length > 500 then
      pure (.error (.validationError "title" "Title must be 500 characters or less"))
    else
      s.ops.updateIssue id f actor
      pure (.ok ())

/-- Add a label with validation and duplicate check -/
def addLabel (s : SafeStorage) (issueId : IssueId) (label : String) (actor : String) : IO (SafeResult Unit) := do
  match validateLabel label with
  | .error e => pure (.error e)
  | .ok validLabel =>
    -- Check issue exists
    match ← s.ops.getIssue issueId with
    | none => pure (.error (.issueNotFound issueId))
    | some issue =>
      -- Check for duplicate
      if issue.labels.contains validLabel.val then
        pure (.error (.duplicateLabel issueId validLabel.val))
      else
        s.ops.addLabel issueId validLabel.val actor
        pure (.ok ())

/-- Remove a label with existence check -/
def removeLabel (s : SafeStorage) (issueId : IssueId) (label : String) (actor : String) : IO (SafeResult Unit) := do
  match ← s.ops.getIssue issueId with
  | none => pure (.error (.issueNotFound issueId))
  | some issue =>
    if !issue.labels.contains label then
      pure (.error (.labelNotFound issueId label))
    else
      s.ops.removeLabel issueId label actor
      pure (.ok ())

/-- Add a dependency with validation -/
def addDependency (s : SafeStorage) (dep : Dependency) (actor : String) : IO (SafeResult Unit) := do
  -- Check for self-dependency
  if dep.issueId == dep.dependsOnId then
    pure (.error (.selfDependency dep.issueId))
  -- Check both issues exist
  else match ← s.ops.getIssue dep.issueId with
  | none => pure (.error (.issueNotFound dep.issueId))
  | some _ =>
    match ← s.ops.getIssue dep.dependsOnId with
    | none => pure (.error (.issueNotFound dep.dependsOnId))
    | some _ =>
      -- Check for cycles
      match ← s.ops.addDependency dep actor with
      | .error (.wouldCreateCycle from_ to path) =>
        pure (.error (.cycleDetected from_ to path))
      | .ok () => pure (.ok ())

/-- Remove a dependency with existence check -/
def removeDependency (s : SafeStorage) (issueId dependsOnId : IssueId) (actor : String) : IO (SafeResult Unit) := do
  let deps ← s.ops.getDependencies issueId
  if deps.any (fun d => d.dependsOnId == dependsOnId) then
    s.ops.removeDependency issueId dependsOnId actor
    pure (.ok ())
  else
    pure (.error (.dependencyNotFound issueId dependsOnId))

/-- Close an issue with validation -/
def closeIssue (s : SafeStorage) (id : IssueId) (reason : String) (actor : String) : IO (SafeResult Unit) := do
  match ← s.ops.getIssue id with
  | none => pure (.error (.issueNotFound id))
  | some issue =>
    if issue.status == .closed then
      pure (.ok ())  -- Already closed, idempotent
    else
      let now ← IO.monoMsNow
      s.ops.updateIssue id (fun i => { i with
        status := .closed
        closedAt := some now
        closeReason := reason
        updatedAt := now
      }) actor
      pure (.ok ())

/-- Reopen an issue with validation -/
def reopenIssue (s : SafeStorage) (id : IssueId) (actor : String) : IO (SafeResult Unit) := do
  match ← s.ops.getIssue id with
  | none => pure (.error (.issueNotFound id))
  | some issue =>
    if issue.status != .closed then
      pure (.ok ())  -- Already open, idempotent
    else
      let now ← IO.monoMsNow
      s.ops.updateIssue id (fun i => { i with
        status := .«open»
        closedAt := none
        closeReason := ""
        updatedAt := now
      }) actor
      pure (.ok ())

/-- Save with error handling -/
def save (s : SafeStorage) : IO (SafeResult Unit) := do
  try
    s.ops.save
    pure (.ok ())
  catch e =>
    pure (.error (.persistenceError (toString e)))

/-- Load with error handling -/
def load (s : SafeStorage) : IO (SafeResult Unit) := do
  try
    s.ops.load
    pure (.ok ())
  catch e =>
    pure (.error (.persistenceError (toString e)))

end SafeStorage

/-- Create a safe storage wrapper -/
def wrapStorage (ops : StorageOps) : SafeStorage := ⟨ops⟩

end Beads
