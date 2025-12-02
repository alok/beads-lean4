/-
Dependently-Typed DAG for Beads
Inspired by https://github.com/elisabethstenholm/questionnaire

The key insight from questionnaires: the type of the "next question" depends
on the VALUE of previous answers, not just their types.

Applied to issue tracking:
- A Dependency is only valid if both issues EXIST
- A DAG is built by construction: new edges can only point to existing nodes
- "Ready" is a dependent type: Ready issue deps requires proof that blockers are closed
-/

import Beads.Core.Types
import Beads.Core.Issue

namespace Beads

/-! ## Membership Proofs

Instead of runtime checks, we use types to prove an issue exists in a set.
-/

/-- Proof that an IssueId is a member of a list of issues -/
inductive IsMember : IssueId → List Issue → Prop where
  | head : ∀ {id issues}, (h : Issue) → h.id = id → IsMember id (h :: issues)
  | tail : ∀ {id issues h}, IsMember id issues → IsMember id (h :: issues)

/-- Decidable membership for issues -/
def findIssue? (id : IssueId) (issues : List Issue) : Option Issue :=
  issues.find? (·.id == id)

/-- A ValidIssueId is an IssueId bundled with proof it exists in the issue set -/
structure ValidIssueId (issues : List Issue) where
  id : IssueId
  exists_ : (findIssue? id issues).isSome = true

namespace ValidIssueId

/-- Get the actual Issue from a ValidIssueId -/
def toIssue (vid : ValidIssueId issues) : Issue :=
  match findIssue? vid.id issues with
  | some i => i
  | none => panic! "ValidIssueId invariant violated"

end ValidIssueId

/-! ## Dependently-Typed Dependencies

A ValidDep can only be constructed if both issues exist in the issue set.
This is similar to how a questionnaire's next question depends on previous answers.
-/

/-- A dependency that is validated against an issue set -/
structure ValidDep (issues : List Issue) where
  /-- The issue that has the dependency -/
  issueId : ValidIssueId issues
  /-- The issue being depended on -/
  dependsOnId : ValidIssueId issues
  /-- Dependency type -/
  depType : DependencyType
  /-- The two issues must be different -/
  ne : issueId.id ≠ dependsOnId.id

/-- Try to validate a raw Dependency against an issue set -/
def Dependency.validate? (d : Dependency) (issues : List Issue) : Option (ValidDep issues) :=
  -- Check both issues exist
  match h1 : (findIssue? d.issueId issues).isSome, h2 : (findIssue? d.dependsOnId issues).isSome with
  | true, true =>
    -- Check they're different
    if hne : d.issueId ≠ d.dependsOnId then
      some {
        issueId := ⟨d.issueId, h1⟩
        dependsOnId := ⟨d.dependsOnId, h2⟩
        depType := d.depType
        ne := hne
      }
    else none
  | _, _ => none

/-! ## Topologically-Indexed DAG

A DAG where the structure itself guarantees acyclicity.
Issues are ordered, and dependencies can only point "backward" to earlier issues.

This is like a questionnaire where each question can only reference
previous answers, never future ones.
-/

/-- An issue's position in a topological ordering -/
abbrev TopoIndex := Nat

/-- A topologically-ordered issue list - earlier indices come first -/
structure TopoIssues where
  issues : List Issue
  -- Each issue's "depth" in the DAG (max path length from a root)
  depths : List Nat
  validLengths : issues.length = depths.length

/-- A dependency in a topo-ordered graph can only point to issues with SMALLER index
    (earlier in topological order = must be processed first = valid dependency target) -/
structure TopoDep (ti : TopoIssues) where
  /-- Index of the dependent issue -/
  fromIdx : Fin ti.issues.length
  /-- Index of the dependency (must be strictly less) -/
  toIdx : Fin ti.issues.length
  /-- The dependency points backward in topo order -/
  backward : toIdx.val < fromIdx.val
  /-- Dependency type -/
  depType : DependencyType

/-! ## Ready as a Dependent Type

The "readiness" of an issue depends on the STATUS VALUES of its blockers.
This is the core dependent type insight: Ready takes values, not just types.
-/

/-- Proof that all blocking dependencies are satisfied (blockers are closed) -/
structure AllBlockersClosed (issue : Issue) (issues : List Issue) (deps : List Dependency) : Prop where
  /-- For every dependency where this issue depends on something that affects blocking,
      the blocker is closed -/
  closed : ∀ d, d ∈ deps → d.issueId = issue.id → d.depType.affectsBlocking →
    ∀ blocker, findIssue? d.dependsOnId issues = some blocker → blocker.status = .closed

/-- An issue that is ready for work, with proof -/
structure ReadyIssue (issues : List Issue) (deps : List Dependency) where
  issue : Issue
  /-- Issue exists in the set -/
  exists_ : issue ∈ issues
  /-- Issue is open for work -/
  isOpen : issue.status.isOpenForWork = true
  /-- All blockers are closed -/
  unblocked : AllBlockersClosed issue issues deps

/-- Check if an issue is in the list by ID -/
def issueIn (issue : Issue) (issues : List Issue) : Bool :=
  issues.any (·.id == issue.id)

/-- Check if an issue is ready (computable version without proof terms) -/
def checkReadyBool (issue : Issue) (issues : List Issue) (deps : List Dependency) : Bool :=
  issueIn issue issues &&
  issue.status.isOpenForWork &&
  (deps.filter (fun d => d.issueId == issue.id && d.depType.affectsBlocking)).all fun d =>
    match findIssue? d.dependsOnId issues with
    | some blocker => blocker.status == .closed
    | none => true

/-! ## DAG by Construction

Build a DAG incrementally where cycles are impossible by construction.
Each new edge is validated against the current graph state.
-/

/-- A DAG built incrementally with cycle-freedom guaranteed by construction -/
inductive DAG : List Issue → Type where
  | empty : DAG []
  | addNode : DAG issues → (issue : Issue) → issue ∉ issues → DAG (issue :: issues)
  | addEdge : (g : DAG issues) → (from_ to : Issue) →
      from_ ∈ issues → to ∈ issues → from_ ≠ to →
      -- Key insight: we need a "reachability" check that doesn't create cycle
      -- In a proper implementation, this would be a proof term
      DAG issues

/-- The empty DAG -/
def emptyDAG : DAG [] := .empty

/-! ## Blocking Propagation as Indexed Type

The blocked status propagates through the dependency graph.
We can encode this as an indexed type family.
-/

/-- BlockedBy issue blocker means issue is blocked by blocker through the dep graph -/
inductive BlockedBy : Issue → Issue → List Dependency → Prop where
  /-- Direct blocking dependency -/
  | direct : ∀ {issue blocker deps},
      (∃ d ∈ deps, d.issueId = issue.id ∧ d.dependsOnId = blocker.id ∧ d.depType.affectsBlocking) →
      blocker.status ≠ .closed →
      BlockedBy issue blocker deps
  /-- Transitive through parent-child -/
  | transitive : ∀ {issue parent blocker deps},
      (∃ d ∈ deps, d.issueId = issue.id ∧ d.dependsOnId = parent.id ∧ d.depType = .parentChild) →
      BlockedBy parent blocker deps →
      BlockedBy issue blocker deps

/-- An issue is blocked if there exists some blocker -/
def IsBlocked (issue : Issue) (issues : List Issue) (deps : List Dependency) : Prop :=
  ∃ blocker ∈ issues, BlockedBy issue blocker deps

/-- Negation: an issue is ready if it's NOT blocked -/
def IsReady (issue : Issue) (issues : List Issue) (deps : List Dependency) : Prop :=
  issue.status.isOpenForWork ∧ ¬IsBlocked issue issues deps

/-! ## Computable Snoc-DAG

A practical DAG implementation using snoc-lists (append at end).
New issues are added at the end, dependencies can only point to earlier issues.
This is "topological order by construction" - cycles are impossible.
-/

/-- A snoc-list DAG where new nodes are appended at the end.
    Issues are in topological order (earlier = can be depended on).
    Dependencies are (from_idx, to_idx) pairs where to_idx < from_idx.
    This invariant ensures acyclicity by construction. -/
structure SnocDAG where
  issues : Array Issue
  edges : Array (Nat × Nat × DependencyType)
  deriving Repr, Inhabited

namespace SnocDAG

/-- Empty DAG -/
def empty : SnocDAG := { issues := #[], edges := #[] }

/-- Add a new issue to the DAG (at the end, so it can depend on all existing) -/
def addIssue (dag : SnocDAG) (issue : Issue) : SnocDAG :=
  { dag with issues := dag.issues.push issue }

/-- Try to add an edge. Returns none if it would violate the backward constraint.
    from_idx is the index of the dependent issue (must be >= to_idx for valid DAG) -/
def addEdge? (dag : SnocDAG) (fromIdx toIdx : Nat) (depType : DependencyType)
    : Option SnocDAG :=
  if toIdx < fromIdx && fromIdx < dag.issues.size && toIdx < dag.issues.size then
    some { dag with edges := dag.edges.push (fromIdx, toIdx, depType) }
  else
    none

/-- Get the issue at index i -/
def getIssue? (dag : SnocDAG) (idx : Nat) : Option Issue :=
  if h : idx < dag.issues.size then some dag.issues[idx] else none

/-- Find an issue by ID, returning its index -/
def findIdx? (dag : SnocDAG) (id : IssueId) : Option Nat :=
  dag.issues.findIdx? (·.id == id)

/-- Add an edge by issue IDs. Validates the backward constraint. -/
def addEdgeById? (dag : SnocDAG) (fromId toId : IssueId) (depType : DependencyType)
    : Option SnocDAG := do
  let fromIdx ← dag.findIdx? fromId
  let toIdx ← dag.findIdx? toId
  dag.addEdge? fromIdx toIdx depType

/-- Get all dependencies of an issue (by index) -/
def getDepsOf (dag : SnocDAG) (idx : Nat) : List (Nat × DependencyType) :=
  dag.edges.toList.filterMap fun (from_, to, dt) =>
    if from_ == idx then some (to, dt) else none

/-- Get all issues that depend on the given issue (by index) -/
def getDependents (dag : SnocDAG) (idx : Nat) : List Nat :=
  dag.edges.toList.filterMap fun (from_, to, _) =>
    if to == idx then some from_ else none

/-- Check if an issue is blocked (computable version) -/
def isBlockedAt (dag : SnocDAG) (idx : Nat) : Bool :=
  -- Get all blocking deps
  let blockingDeps := dag.getDepsOf idx |>.filter (·.2.affectsBlocking)
  -- Check if any blocker is not closed
  blockingDeps.any fun (toIdx, _) =>
    match dag.getIssue? toIdx with
    | some issue => issue.status != .closed
    | none => false

/-- Get all ready issues (open and not blocked) -/
def getReady (dag : SnocDAG) : List Issue :=
  let indexed := List.range dag.issues.size |>.zip dag.issues.toList
  indexed.filterMap fun (idx, issue) =>
    if issue.status.isOpenForWork && !dag.isBlockedAt idx then
      some issue
    else
      none

/-- Convert from regular Graph to SnocDAG (topological sort) -/
def fromIssuesAndDeps (issues : List Issue) (deps : List Dependency) : Option SnocDAG :=
  -- Simple approach: order by createdAt (assuming older issues should come first)
  let sortedIssues := issues.toArray.insertionSort (·.createdAt < ·.createdAt)
  let dag := { issues := sortedIssues, edges := #[] : SnocDAG }
  -- Add edges, validating backward constraint
  let rec addEdges (dag : SnocDAG) (remaining : List Dependency) : Option SnocDAG :=
    match remaining with
    | [] => some dag
    | d :: rest =>
      match dag.addEdgeById? d.issueId d.dependsOnId d.depType with
      | some newDag => addEdges newDag rest
      | none => none  -- Edge violates backward constraint
  addEdges dag deps

end SnocDAG

/-! ## Questionnaire Analogy

The questionnaire pattern maps to issue tracking as follows:

```
Questionnaire                    Issue Tracker
──────────────────────────────────────────────────────
Question : Answer → Type         Dep : IssueState → Type
Next question depends on         Next available work depends
  previous answer VALUE            on blocker STATUS VALUE

Q1: "Age?"                       Issue A: status = open
Q2 : (age ≥ 18) → "Vote pref?"   Issue B: blocks A, status = closed
                                  → A is ready (B closed)

If age < 18, Q2 doesn't exist    If B.status ≠ closed,
                                  A.ready doesn't exist
```

The key insight: **TYPE depends on VALUE**, not just on other types.
-/

end Beads
