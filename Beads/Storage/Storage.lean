/-
Beads Storage Interface
Defines the abstract storage operations
-/
import Beads.Core.Json

namespace Beads

/-- Error type for cycle detection -/
inductive CycleError where
  | wouldCreateCycle (from_ to : IssueId) (path : List IssueId)
  deriving Repr

instance : ToString CycleError where
  toString
    | .wouldCreateCycle from_ to path =>
      s!"Adding dependency from {from_} to {to} would create a cycle: {path}"

/-- Storage operations interface (as a structure of functions) -/
structure StorageOps where
  -- Issue operations
  createIssue : Issue → String → IO IssueId
  getIssue : IssueId → IO (Option Issue)
  updateIssue : IssueId → (Issue → Issue) → String → IO Unit
  deleteIssue : IssueId → IO Unit
  getAllIssues : IO (List Issue)
  searchIssues : IssueFilter → IO (List Issue)

  -- Dependency operations
  addDependency : Dependency → String → IO (Except CycleError Unit)
  removeDependency : IssueId → IssueId → String → IO Unit
  getDependencies : IssueId → IO (List Dependency)
  getDependents : IssueId → IO (List Dependency)
  getAllDependencies : IO (List Dependency)

  -- Label operations
  addLabel : IssueId → String → String → IO Unit
  removeLabel : IssueId → String → String → IO Unit
  getLabels : IssueId → IO (List String)

  -- Comment operations
  addComment : IssueId → String → String → IO Nat  -- Returns comment ID
  getComments : IssueId → IO (List Comment)
  getAllComments : IO (List Comment)

  -- Message operations (inter-agent communication)
  sendMessage : String → String → String → String → Option IssueId → IO Nat  -- from, to, subject, body, relatedIssue → message ID
  getInbox : String → IO (List Message)  -- Get messages for recipient
  getMessage : Nat → IO (Option Message)  -- Get message by ID
  markRead : Nat → IO Unit  -- Mark message as read
  markAcked : Nat → IO Unit  -- Mark message as acknowledged
  getAllMessages : IO (List Message)

  -- Ready work operations
  getReadyWork : WorkFilter → IO (List Issue)
  getBlockedIssues : IO (List BlockedIssue)

  -- Cycle detection
  wouldCreateCycle : IssueId → IssueId → IO Bool
  detectCycles : IO (List (List IssueId))

  -- Persistence
  save : IO Unit
  load : IO Unit

end Beads
