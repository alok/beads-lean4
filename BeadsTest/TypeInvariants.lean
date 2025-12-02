/-
Type Invariant Property Tests
Tests for Priority bounds, Status transitions, and Issue validation
-/
import BeadsTest.Arbitrary
import Plausible.Testable
import Beads.Core.Issue
import Beads.Core.Types

open Plausible
open Beads

-- Helper functions for cleaner tests
def statusRoundtrips (s : Status) : Bool :=
  match Status.fromString? s.toString with
  | some s' => s' == s
  | none => false

def issueTypeRoundtrips (t : IssueType) : Bool :=
  match IssueType.fromString? t.toString with
  | some t' => t' == t
  | none => false

def dependencyTypeRoundtrips (d : DependencyType) : Bool :=
  match DependencyType.fromString? d.toString with
  | some d' => d' == d
  | none => false

def sortPolicyRoundtrips (sp : SortPolicy) : Bool :=
  match SortPolicy.fromString? sp.toString with
  | some sp' => sp' == sp
  | none => false

def priorityFromNatWorks (n : Nat) : Bool :=
  match Priority.fromNat? n with
  | some p => n < 5 && p.val == n
  | none => n >= 5

def closedIssueHasTimestamp (issue : Issue) : Bool :=
  if issue.status == .closed then issue.closedAt.isSome
  else true

def nonClosedIssueHasNoTimestamp (issue : Issue) : Bool :=
  if issue.status != .closed then issue.closedAt.isNone
  else true

def issueEmptyTitleFails (issue : Issue) : Bool :=
  if issue.title.isEmpty then
    match Issue.validate { issue with title := "" } with
    | .error _ => true
    | .ok _ => false
  else true  -- Skip non-empty titles

def childIdRespectMaxDepth (parentId : IssueId) : Bool :=
  match generateChildId parentId 1 with
  | .ok childId => childId.depth <= IssueId.maxDepth
  | .error _ => parentId.depth >= IssueId.maxDepth

-- ============================================================================
-- Priority Invariants
-- ============================================================================

-- Priority value is always less than 5 (Fin 5 enforces this at type level)
#eval Testable.check (∀ p : Priority, p.val < 5)
  (cfg := { numInst := 100, quiet := true })

-- Priority.default is P2
#eval Testable.check (Priority.default.val == 2)
  (cfg := { numInst := 1, quiet := true })

-- Priority.fromNat? accepts only 0-4
#eval Testable.check (∀ n : Nat, priorityFromNatWorks n)
  (cfg := { numInst := 100, quiet := true })

-- ============================================================================
-- Status Invariants
-- ============================================================================

-- Status.fromString? roundtrips with toString
#eval Testable.check (∀ s : Status, statusRoundtrips s)
  (cfg := { numInst := 100, quiet := true })

-- Status.isOpenForWork is true only for open and inProgress
#eval Testable.check (∀ s : Status, s.isOpenForWork == (s == .«open» || s == .inProgress))
  (cfg := { numInst := 100, quiet := true })

-- ============================================================================
-- IssueType Invariants
-- ============================================================================

-- IssueType.fromString? roundtrips with toString
#eval Testable.check (∀ t : IssueType, issueTypeRoundtrips t)
  (cfg := { numInst := 100, quiet := true })

-- ============================================================================
-- DependencyType Invariants
-- ============================================================================

-- DependencyType.fromString? roundtrips with toString
#eval Testable.check (∀ d : DependencyType, dependencyTypeRoundtrips d)
  (cfg := { numInst := 100, quiet := true })

-- Only blocks and parentChild affect blocking
#eval Testable.check (∀ d : DependencyType, d.affectsBlocking == (d == .blocks || d == .parentChild))
  (cfg := { numInst := 100, quiet := true })

-- ============================================================================
-- Issue Validation Invariants
-- ============================================================================

-- Closed issues must have closedAt timestamp
#eval Testable.check (∀ issue : Issue, closedIssueHasTimestamp issue)
  (cfg := { numInst := 100, quiet := true })

-- Non-closed issues must not have closedAt timestamp
#eval Testable.check (∀ issue : Issue, nonClosedIssueHasNoTimestamp issue)
  (cfg := { numInst := 100, quiet := true })

-- Issues with empty title fail validation
#eval Testable.check (∀ issue : Issue, issueEmptyTitleFails issue)
  (cfg := { numInst := 50, quiet := true })

-- ============================================================================
-- IssueId Invariants
-- ============================================================================

-- IssueId depth is always <= maxDepth for valid generated IDs
#eval Testable.check (∀ parentId : IssueId, childIdRespectMaxDepth parentId)
  (cfg := { numInst := 100, quiet := true })

-- parseHierarchy is consistent with depth
#eval Testable.check (∀ id : IssueId,
  let (_, _, depth) := id.parseHierarchy
  depth == id.depth)
  (cfg := { numInst := 100, quiet := true })

-- ============================================================================
-- SortPolicy Invariants
-- ============================================================================

-- SortPolicy.fromString? roundtrips for valid inputs
#eval Testable.check (∀ sp : SortPolicy, sortPolicyRoundtrips sp)
  (cfg := { numInst := 100, quiet := true })
