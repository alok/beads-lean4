/-
Beads Core Types
Port of internal/types/types.go
-/

namespace Beads

/-- Issue status - represents the current state of an issue -/
inductive Status where
  | «open»
  | inProgress
  | blocked
  | closed
  deriving DecidableEq, Repr, Hashable, Inhabited

namespace Status

def toString : Status → String
  | .«open» => "open"
  | .inProgress => "in_progress"
  | .blocked => "blocked"
  | .closed => "closed"

def fromString? : String → Option Status
  | "open" => some .«open»
  | "in_progress" => some .inProgress
  | "blocked" => some .blocked
  | "closed" => some .closed
  | _ => none

def isValid (s : String) : Bool := (fromString? s).isSome

instance : ToString Status := ⟨toString⟩

/-- Status is open for work (open or in_progress) -/
def isOpenForWork : Status → Bool
  | .«open» => true
  | .inProgress => true
  | _ => false

end Status

/-- Issue type - categorizes the kind of work -/
inductive IssueType where
  | bug
  | feature
  | task
  | epic
  | chore
  deriving DecidableEq, Repr, Hashable, Inhabited

namespace IssueType

def toString : IssueType → String
  | .bug => "bug"
  | .feature => "feature"
  | .task => "task"
  | .epic => "epic"
  | .chore => "chore"

def fromString? : String → Option IssueType
  | "bug" => some .bug
  | "feature" => some .feature
  | "task" => some .task
  | "epic" => some .epic
  | "chore" => some .chore
  | _ => none

def isValid (s : String) : Bool := (fromString? s).isSome

instance : ToString IssueType := ⟨toString⟩

end IssueType

/-- Dependency type - categorizes the relationship between issues -/
inductive DependencyType where
  | blocks         -- A blocks B: B cannot start until A is closed
  | related        -- Informational link, no blocking semantics
  | parentChild    -- B is a subtask of A (epic/task relationship)
  | discoveredFrom -- B was discovered while working on A
  deriving DecidableEq, Repr, Hashable, Inhabited

namespace DependencyType

def toString : DependencyType → String
  | .blocks => "blocks"
  | .related => "related"
  | .parentChild => "parent-child"
  | .discoveredFrom => "discovered-from"

def fromString? : String → Option DependencyType
  | "blocks" => some .blocks
  | "related" => some .related
  | "parent-child" => some .parentChild
  | "discovered-from" => some .discoveredFrom
  | _ => none

def isValid (s : String) : Bool := (fromString? s).isSome

instance : ToString DependencyType := ⟨toString⟩

/-- Whether this dependency type affects blocking propagation -/
def affectsBlocking : DependencyType → Bool
  | .blocks => true
  | .parentChild => true  -- Children inherit parent's blocked status
  | _ => false

end DependencyType

/-- Priority (0-4, where 0 is highest priority) -/
abbrev Priority := Fin 5

namespace Priority

def default : Priority := ⟨2, by decide⟩

def toString (p : Priority) : String := s!"P{p.val}"

def fromNat? (n : Nat) : Option Priority :=
  if h : n < 5 then some ⟨n, h⟩ else none

instance : ToString Priority := ⟨toString⟩
instance : Inhabited Priority := ⟨default⟩

end Priority

/-- Event type for audit trail -/
inductive EventType where
  | created
  | updated
  | statusChanged
  | commented
  | closed
  | reopened
  | dependencyAdded
  | dependencyRemoved
  | labelAdded
  | labelRemoved
  | compacted
  deriving DecidableEq, Repr, Hashable, Inhabited

namespace EventType

def toString : EventType → String
  | .created => "created"
  | .updated => "updated"
  | .statusChanged => "status_changed"
  | .commented => "commented"
  | .closed => "closed"
  | .reopened => "reopened"
  | .dependencyAdded => "dependency_added"
  | .dependencyRemoved => "dependency_removed"
  | .labelAdded => "label_added"
  | .labelRemoved => "label_removed"
  | .compacted => "compacted"

instance : ToString EventType := ⟨toString⟩

end EventType

/-- Sort policy for ready work -/
inductive SortPolicy where
  | hybrid    -- Recent by priority, older by age (default)
  | priority  -- Always by priority first
  | oldest    -- Always by creation date (oldest first)
  deriving DecidableEq, Repr, Hashable, Inhabited

namespace SortPolicy

def toString : SortPolicy → String
  | .hybrid => "hybrid"
  | .priority => "priority"
  | .oldest => "oldest"

def fromString? : String → Option SortPolicy
  | "hybrid" => some .hybrid
  | "priority" => some .priority
  | "oldest" => some .oldest
  | "" => some .hybrid  -- default
  | _ => none

instance : ToString SortPolicy := ⟨toString⟩

end SortPolicy

end Beads
