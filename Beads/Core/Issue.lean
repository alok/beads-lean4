/-
Beads Issue Structure
Port of internal/types/types.go Issue struct
-/
import Beads.Core.Types
import Lean.Data.Json

namespace Beads

/-- Issue ID - wrapper for collision-resistant hash-based IDs -/
structure IssueId where
  value : String
  deriving DecidableEq, Repr, Hashable, Inhabited, Ord

namespace IssueId

instance : ToString IssueId := ⟨fun id => id.value⟩
instance : BEq IssueId := ⟨fun a b => a.value == b.value⟩

/-- Check if this is a hierarchical (child) ID -/
def isHierarchical (id : IssueId) : Bool :=
  id.value.contains '.'

/-- Parse hierarchical ID to get (rootId, parentId?, depth)
    Examples:
      "bd-af78e9a2" → ("bd-af78e9a2", none, 0)
      "bd-af78e9a2.1" → ("bd-af78e9a2", some "bd-af78e9a2", 1)
      "bd-af78e9a2.1.2" → ("bd-af78e9a2", some "bd-af78e9a2.1", 2)
-/
def parseHierarchy (id : IssueId) : (IssueId × Option IssueId × Nat) :=
  let parts := id.value.splitOn "."
  match parts with
  | [root] => (⟨root⟩, none, 0)
  | root :: rest =>
    let depth := rest.length
    let parentParts := parts.take (parts.length - 1)
    let parent := ".".intercalate parentParts
    (⟨root⟩, some ⟨parent⟩, depth)
  | [] => (id, none, 0)  -- shouldn't happen

/-- Get the root ID (strips all .N suffixes) -/
def rootId (id : IssueId) : IssueId :=
  (parseHierarchy id).1

/-- Get the parent ID if this is a child issue -/
def parentId? (id : IssueId) : Option IssueId :=
  (parseHierarchy id).2.1

/-- Get the hierarchy depth (0 for root issues) -/
def depth (id : IssueId) : Nat :=
  (parseHierarchy id).2.2

/-- Maximum hierarchy depth (prevents over-decomposition) -/
def maxDepth : Nat := 3

end IssueId

/-- Timestamp as Unix milliseconds -/
abbrev Timestamp := Nat

namespace Timestamp

def now : IO Timestamp := do
  let t ← IO.monoMsNow
  pure t

/-- Format as ISO 8601 (simplified - just for display) -/
def toIsoString (ts : Timestamp) : String :=
  -- Simple placeholder - in real code would do proper date formatting
  s!"{ts}"

end Timestamp

/-- Issue represents a trackable work item -/
structure Issue where
  id : IssueId
  title : String
  description : String := ""
  design : String := ""
  acceptanceCriteria : String := ""
  notes : String := ""
  status : Status := .«open»
  priority : Priority := Priority.default
  issueType : IssueType := .task
  assignee : Option String := none
  estimatedMinutes : Option Nat := none
  createdAt : Timestamp
  updatedAt : Timestamp
  closedAt : Option Timestamp := none
  closeReason : String := ""
  externalRef : Option String := none
  labels : List String := []
  deriving Repr, Inhabited

namespace Issue

/-- Validate issue fields -/
def validate (issue : Issue) : Except String Unit := do
  if issue.title.isEmpty then
    throw "title is required"
  if issue.title.length > 500 then
    throw s!"title must be 500 characters or less (got {issue.title.length})"
  -- Priority is enforced by Fin 5 type
  -- Status is enforced by Status type
  -- IssueType is enforced by IssueType type
  if let some est := issue.estimatedMinutes then
    if est == 0 then
      pure ()  -- 0 is OK, means "unknown"
  -- closedAt invariant: must be set iff status is closed
  match issue.status, issue.closedAt with
  | .closed, none => throw "closed issues must have closed_at timestamp"
  | s, some _ => if s != .closed then throw "non-closed issues cannot have closed_at timestamp" else pure ()
  | _, _ => pure ()

/-- Create a new issue with current timestamp -/
def create (title : String) (issueType : IssueType := .task) (priority : Priority := Priority.default) : IO Issue := do
  let now ← Timestamp.now
  pure {
    id := ⟨""⟩  -- Will be set by ID generator
    title := title
    issueType := issueType
    priority := priority
    createdAt := now
    updatedAt := now
  }

/-- Update the issue, bumping updatedAt -/
def update (issue : Issue) (f : Issue → Issue) : IO Issue := do
  let now ← Timestamp.now
  let updated := f issue
  pure { updated with updatedAt := now }

/-- Close the issue -/
def close (issue : Issue) (reason : String := "") : IO Issue := do
  let now ← Timestamp.now
  pure { issue with
    status := .closed
    closedAt := some now
    closeReason := reason
    updatedAt := now
  }

/-- Reopen a closed issue -/
def reopen (issue : Issue) : IO Issue := do
  let now ← Timestamp.now
  pure { issue with
    status := .«open»
    closedAt := none
    closeReason := ""
    updatedAt := now
  }

end Issue

/-- Dependency represents a relationship between issues -/
structure Dependency where
  issueId : IssueId      -- The issue that has the dependency
  dependsOnId : IssueId  -- The issue being depended on
  depType : DependencyType
  createdAt : Timestamp
  createdBy : String
  deriving Repr, Inhabited

namespace Dependency

/-- Create a new dependency -/
def create (issueId dependsOnId : IssueId) (depType : DependencyType) (actor : String) : IO Dependency := do
  let now ← Timestamp.now
  pure {
    issueId := issueId
    dependsOnId := dependsOnId
    depType := depType
    createdAt := now
    createdBy := actor
  }

end Dependency

/-- Comment on an issue -/
structure Comment where
  id : Nat
  issueId : IssueId
  author : String
  text : String
  createdAt : Timestamp
  deriving Repr, Inhabited

/-- Event for audit trail -/
structure Event where
  id : Nat
  issueId : IssueId
  eventType : EventType
  actor : String
  oldValue : Option String := none
  newValue : Option String := none
  comment : Option String := none
  createdAt : Timestamp
  deriving Repr, Inhabited

/-- Label on an issue -/
structure Label where
  issueId : IssueId
  label : String
  deriving Repr, Inhabited, DecidableEq

/-- BlockedIssue extends Issue with blocking information -/
structure BlockedIssue where
  issue : Issue
  blockedByCount : Nat
  blockedBy : List IssueId
  deriving Repr, Inhabited

/-- TreeNode for dependency tree visualization -/
structure TreeNode where
  issue : Issue
  depth : Nat
  parentId : Option IssueId
  truncated : Bool := false
  deriving Repr, Inhabited

/-- Statistics for aggregate metrics -/
structure Statistics where
  totalIssues : Nat := 0
  openIssues : Nat := 0
  inProgressIssues : Nat := 0
  closedIssues : Nat := 0
  blockedIssues : Nat := 0
  readyIssues : Nat := 0
  epicsEligibleForClosure : Nat := 0
  deriving Repr, Inhabited

/-- IssueFilter for querying issues -/
structure IssueFilter where
  status : Option Status := none
  priority : Option Priority := none
  issueType : Option IssueType := none
  assignee : Option String := none
  labels : List String := []      -- AND semantics
  labelsAny : List String := []   -- OR semantics
  titleSearch : String := ""
  ids : List IssueId := []
  limit : Nat := 100
  deriving Repr, Inhabited

namespace IssueFilter
def default : IssueFilter := {}
end IssueFilter

/-- WorkFilter for ready work queries -/
structure WorkFilter where
  status : Option Status := none   -- Filter by status (default: open or in_progress)
  priority : Option Priority := none
  assignee : Option String := none
  unassigned : Bool := false       -- Filter for issues with no assignee
  labels : List String := []       -- AND semantics
  labelsAny : List String := []    -- OR semantics
  limit : Nat := 10
  sortPolicy : SortPolicy := .hybrid
  deriving Repr, Inhabited

namespace WorkFilter
def default : WorkFilter := {}
end WorkFilter

end Beads
