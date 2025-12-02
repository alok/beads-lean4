/-
Blocking Computation Property Tests
Tests for blocked/ready issue computation
-/
import BeadsTest.Arbitrary
import Plausible.Testable
import Beads.Core.Graph

open Plausible
open Beads

-- ============================================================================
-- Test Helpers
-- ============================================================================

/-- Create a simple open issue with given ID -/
def mkOpenIssue (idStr : String) (title : String := "Test") : Issue :=
  { id := ⟨idStr⟩
    title := title
    description := ""
    status := .open
    priority := Priority.default
    issueType := .task
    createdAt := 0
    updatedAt := 0 }

/-- Create a closed issue with given ID -/
def mkClosedIssue (idStr : String) (title : String := "Test") : Issue :=
  { id := ⟨idStr⟩
    title := title
    description := ""
    status := .closed
    priority := Priority.default
    issueType := .task
    createdAt := 0
    updatedAt := 0
    closedAt := some 1 }

/-- Create a blocking dependency (A blocks B means B depends on A) -/
def mkBlocksDep (blockedId blockerId : String) : Dependency :=
  { issueId := ⟨blockedId⟩
    dependsOnId := ⟨blockerId⟩
    depType := .blocks
    createdAt := 0
    createdBy := "test" }

/-- Create a parent-child dependency (child depends on parent) -/
def mkParentChildDep (childId parentId : String) : Dependency :=
  { issueId := ⟨childId⟩
    dependsOnId := ⟨parentId⟩
    depType := .parentChild
    createdAt := 0
    createdBy := "test" }

/-- Create a related dependency (no blocking) -/
def mkRelatedDep (fromId toId : String) : Dependency :=
  { issueId := ⟨fromId⟩
    dependsOnId := ⟨toId⟩
    depType := .related
    createdAt := 0
    createdBy := "test" }

-- ============================================================================
-- Basic Blocking Properties
-- ============================================================================

-- No deps means no blocked issues
def noDepsMeansNoBlocked (issues : List Issue) : Bool :=
  (computeBlockedSet issues []).isEmpty

-- Issue blocked by open issue is in blocked set
def blockedByOpenIsBlocked : Bool :=
  let a := mkOpenIssue "bd-aaa"
  let b := mkOpenIssue "bd-bbb"
  let dep := mkBlocksDep "bd-bbb" "bd-aaa"  -- B is blocked by A
  isBlocked [a, b] [dep] ⟨"bd-bbb"⟩

-- Issue blocked by closed issue is NOT blocked
def blockedByClosedNotBlocked : Bool :=
  let a := mkClosedIssue "bd-aaa"
  let b := mkOpenIssue "bd-bbb"
  let dep := mkBlocksDep "bd-bbb" "bd-aaa"  -- B is blocked by A (but A is closed)
  !isBlocked [a, b] [dep] ⟨"bd-bbb"⟩

-- Related dependencies don't cause blocking
def relatedDepsNoBlocking : Bool :=
  let a := mkOpenIssue "bd-aaa"
  let b := mkOpenIssue "bd-bbb"
  let dep := mkRelatedDep "bd-bbb" "bd-aaa"  -- B related to A (not blocking)
  !isBlocked [a, b] [dep] ⟨"bd-bbb"⟩

-- Self-blocking with open issue
def selfBlockingWorks : Bool :=
  let a := mkOpenIssue "bd-aaa"
  let dep := mkBlocksDep "bd-aaa" "bd-aaa"  -- A blocked by itself
  isBlocked [a] [dep] ⟨"bd-aaa"⟩

-- ============================================================================
-- Parent-Child Blocking Propagation
-- ============================================================================

-- Parent-child dep blocks child when parent is open
-- (parentChild.affectsBlocking = true)
def parentChildBlocksWhenParentOpen : Bool :=
  let parent := mkOpenIssue "bd-parent"
  let child := mkOpenIssue "bd-child"
  let childDep := mkParentChildDep "bd-child" "bd-parent"
  isBlocked [parent, child] [childDep] ⟨"bd-child"⟩

-- Child not blocked if parent is closed
def childNotBlockedIfParentClosed : Bool :=
  let parent := mkClosedIssue "bd-parent"
  let child := mkOpenIssue "bd-child"
  let childDep := mkParentChildDep "bd-child" "bd-parent"
  !isBlocked [parent, child] [childDep] ⟨"bd-child"⟩

-- Transitive: blocked parent propagates to child
-- (blocking propagates one level through parent-child)
def blockedParentBlocksChild : Bool :=
  let parent := mkOpenIssue "bd-parent"
  let child := mkOpenIssue "bd-child"
  let blocker := mkOpenIssue "bd-blocker"
  -- Parent is blocked by blocker
  let blockDep := mkBlocksDep "bd-parent" "bd-blocker"
  -- Child is a child of parent
  let childDep := mkParentChildDep "bd-child" "bd-parent"
  -- Child is blocked because parent is blocked
  isBlocked [parent, child, blocker] [blockDep, childDep] ⟨"bd-child"⟩

-- ============================================================================
-- Ready Issue Properties
-- ============================================================================

-- Issue with no blocking deps is ready
def unblockIssueIsReady : Bool :=
  let a := mkOpenIssue "bd-aaa"
  let ready := getReadyIssues [a] []
  ready.length == 1 && ready.any (·.id == ⟨"bd-aaa"⟩)

-- Closed issue is not ready (even if unblocked)
def closedIssueNotReady : Bool :=
  let a := mkClosedIssue "bd-aaa"
  let ready := getReadyIssues [a] []
  ready.isEmpty

-- Blocked issue is not ready
def blockedIssueNotReady : Bool :=
  let a := mkOpenIssue "bd-aaa"
  let b := mkOpenIssue "bd-bbb"
  let dep := mkBlocksDep "bd-bbb" "bd-aaa"
  let ready := getReadyIssues [a, b] [dep]
  -- a is ready, b is not
  ready.length == 1 && ready.any (·.id == ⟨"bd-aaa"⟩)

-- Closing blocker makes blocked issue ready
def closingBlockerMakesReady : Bool :=
  let a := mkClosedIssue "bd-aaa"  -- Blocker is now closed
  let b := mkOpenIssue "bd-bbb"
  let dep := mkBlocksDep "bd-bbb" "bd-aaa"
  let ready := getReadyIssues [a, b] [dep]
  -- b is now ready since a is closed
  ready.length == 1 && ready.any (·.id == ⟨"bd-bbb"⟩)

-- ============================================================================
-- Property-Based Tests with Generated Data
-- ============================================================================

-- No blocking deps means all open issues are ready
def noBlockingAllOpenReady (issues : List Issue) : Bool :=
  let openIssues := issues.filter (·.status.isOpenForWork)
  let ready := getReadyIssues issues []
  ready.length == openIssues.length

-- Closed issues never in blocked set
def closedNeverBlocked (issues : List Issue) (deps : List Dependency) : Bool :=
  let blocked := computeBlockedSet issues deps
  let closedIds := issues.filter (·.status == .closed) |>.map (·.id)
  closedIds.all fun id => !blocked.contains id

-- ============================================================================
-- Property Tests
-- ============================================================================

-- Basic blocking
#eval Testable.check (∀ issues : List Issue, noDepsMeansNoBlocked issues)
  (cfg := { numInst := 30, quiet := true })

#eval Testable.check blockedByOpenIsBlocked
  (cfg := { numInst := 1, quiet := true })

#eval Testable.check blockedByClosedNotBlocked
  (cfg := { numInst := 1, quiet := true })

#eval Testable.check relatedDepsNoBlocking
  (cfg := { numInst := 1, quiet := true })

#eval Testable.check selfBlockingWorks
  (cfg := { numInst := 1, quiet := true })

-- Parent-child blocking
#eval Testable.check parentChildBlocksWhenParentOpen
  (cfg := { numInst := 1, quiet := true })

#eval Testable.check childNotBlockedIfParentClosed
  (cfg := { numInst := 1, quiet := true })

#eval Testable.check blockedParentBlocksChild
  (cfg := { numInst := 1, quiet := true })

-- Ready issues
#eval Testable.check unblockIssueIsReady
  (cfg := { numInst := 1, quiet := true })

#eval Testable.check closedIssueNotReady
  (cfg := { numInst := 1, quiet := true })

#eval Testable.check blockedIssueNotReady
  (cfg := { numInst := 1, quiet := true })

#eval Testable.check closingBlockerMakesReady
  (cfg := { numInst := 1, quiet := true })

-- Property-based with generated data
#eval Testable.check (∀ issues : List Issue, noBlockingAllOpenReady issues)
  (cfg := { numInst := 30, quiet := true })

#eval Testable.check (∀ issues : List Issue, ∀ deps : List Dependency, closedNeverBlocked issues deps)
  (cfg := { numInst := 30, quiet := true })

