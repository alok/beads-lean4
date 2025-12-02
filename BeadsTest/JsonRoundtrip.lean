/-
JSON Roundtrip Property Tests
Tests that toJson → fromJson = identity for all serializable types
-/
import BeadsTest.Arbitrary
import Plausible.Testable
import Beads.Core.Json

open Plausible
open Beads
open Lean Json

-- Helper to check JSON roundtrip
def jsonRoundtrips [ToJson α] [FromJson α] [BEq α] (x : α) : Bool :=
  match fromJson? (toJson x) with
  | .ok y => x == y
  | .error _ => false

-- Helper to check Issue core fields roundtrip
def issueFieldsRoundtrip (issue : Issue) : Bool :=
  match fromJson? (α := Issue) (toJson issue) with
  | .ok decoded =>
    decoded.id == issue.id &&
    decoded.title == issue.title &&
    decoded.status == issue.status &&
    decoded.priority == issue.priority &&
    decoded.issueType == issue.issueType &&
    decoded.createdAt == issue.createdAt &&
    decoded.updatedAt == issue.updatedAt
  | .error _ => false

-- Helper to check Issue closedAt roundtrip
def issueClosedAtRoundtrip (issue : Issue) : Bool :=
  match fromJson? (α := Issue) (toJson issue) with
  | .ok decoded => decoded.closedAt == issue.closedAt
  | .error _ => false

-- Helper to check Dependency roundtrip
def dependencyRoundtrip (dep : Dependency) : Bool :=
  match fromJson? (α := Dependency) (toJson dep) with
  | .ok decoded =>
    decoded.issueId == dep.issueId &&
    decoded.dependsOnId == dep.dependsOnId &&
    decoded.depType == dep.depType &&
    decoded.createdAt == dep.createdAt &&
    decoded.createdBy == dep.createdBy
  | .error _ => false

-- Helper to check if JSON has expected keys
def issueHasKeys (issue : Issue) : Bool :=
  let json := toJson issue
  match json with
  | .obj fields =>
    fields.contains "id" &&
    fields.contains "title" &&
    fields.contains "status" &&
    fields.contains "priority" &&
    fields.contains "issue_type" &&
    fields.contains "created_at" &&
    fields.contains "updated_at"
  | _ => false

def dependencyHasKeys (dep : Dependency) : Bool :=
  let json := toJson dep
  match json with
  | .obj fields =>
    fields.contains "issue_id" &&
    fields.contains "depends_on_id" &&
    fields.contains "type" &&
    fields.contains "created_at" &&
    fields.contains "created_by"
  | _ => false

-- Helper for invalid JSON tests
def invalidStatusRejected : Bool :=
  match fromJson? (α := Status) (.str "not_a_status") with
  | .ok _ => false
  | .error _ => true

def invalidPriority10Rejected : Bool :=
  match fromJson? (α := Priority) (.num 10) with
  | .ok _ => false
  | .error _ => true

def priority0Valid : Bool :=
  match fromJson? (α := Priority) (.num 0) with
  | .ok p => p.val == 0
  | .error _ => false

def priority4Valid : Bool :=
  match fromJson? (α := Priority) (.num 4) with
  | .ok p => p.val == 4
  | .error _ => false

def priority5Invalid : Bool :=
  match fromJson? (α := Priority) (.num 5) with
  | .ok _ => false
  | .error _ => true

-- ============================================================================
-- Simple Type Roundtrips
-- ============================================================================

-- Status JSON roundtrip
#eval Testable.check (∀ s : Status, jsonRoundtrips s)
  (cfg := { numInst := 100, quiet := true })

-- IssueType JSON roundtrip
#eval Testable.check (∀ t : IssueType, jsonRoundtrips t)
  (cfg := { numInst := 100, quiet := true })

-- DependencyType JSON roundtrip
#eval Testable.check (∀ d : DependencyType, jsonRoundtrips d)
  (cfg := { numInst := 100, quiet := true })

-- Priority JSON roundtrip
#eval Testable.check (∀ p : Priority, jsonRoundtrips p)
  (cfg := { numInst := 100, quiet := true })

-- IssueId JSON roundtrip
#eval Testable.check (∀ id : IssueId, jsonRoundtrips id)
  (cfg := { numInst := 100, quiet := true })

-- ============================================================================
-- Complex Type Roundtrips
-- ============================================================================

-- Issue JSON roundtrip preserves core fields
#eval Testable.check (∀ issue : Issue, issueFieldsRoundtrip issue)
  (cfg := { numInst := 50, quiet := true })

-- Issue closedAt field preserves correctly
#eval Testable.check (∀ issue : Issue, issueClosedAtRoundtrip issue)
  (cfg := { numInst := 50, quiet := true })

-- Dependency JSON roundtrip
#eval Testable.check (∀ dep : Dependency, dependencyRoundtrip dep)
  (cfg := { numInst := 50, quiet := true })

-- ============================================================================
-- Invalid JSON Rejection
-- ============================================================================

-- Invalid status string is rejected
#eval Testable.check invalidStatusRejected
  (cfg := { numInst := 1, quiet := true })

-- Invalid priority number is rejected
#eval Testable.check invalidPriority10Rejected
  (cfg := { numInst := 1, quiet := true })

-- Priority must be non-negative (tested with 0 which is valid)
#eval Testable.check priority0Valid
  (cfg := { numInst := 1, quiet := true })

-- Priority 4 is valid (max)
#eval Testable.check priority4Valid
  (cfg := { numInst := 1, quiet := true })

-- Priority 5 is invalid
#eval Testable.check priority5Invalid
  (cfg := { numInst := 1, quiet := true })

-- ============================================================================
-- JSON Structure Tests
-- ============================================================================

-- Issue JSON has expected keys
#eval Testable.check (∀ issue : Issue, issueHasKeys issue)
  (cfg := { numInst := 50, quiet := true })

-- Dependency JSON has expected keys
#eval Testable.check (∀ dep : Dependency, dependencyHasKeys dep)
  (cfg := { numInst := 50, quiet := true })
