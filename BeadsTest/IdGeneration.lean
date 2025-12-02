/-
ID Generation Property Tests
Tests for hash-based collision-resistant ID generation
-/
import BeadsTest.Arbitrary
import Plausible.Testable
import Beads.Core.Id

open Plausible
open Beads

-- Helper functions for cleaner tests
def idStartsWithPrefix (title desc : String) (ts : Timestamp) : Bool :=
  let hash := generateFullHash title desc ts
  let id := allocateIdFromHash [] hash
  id.startsWith "bd-"

def idHasMinLength (title desc : String) (ts : Timestamp) : Bool :=
  let hash := generateFullHash title desc ts
  let id := allocateIdFromHash [] hash
  id.length >= 9  -- "bd-" (3) + 6 hex chars

def hashIsDeterministic (title desc : String) (ts : Timestamp) : Bool :=
  let hash1 := generateFullHash title desc ts
  let hash2 := generateFullHash title desc ts
  hash1 == hash2

def rootIdHasDepthZero (id : IssueId) : Bool :=
  if !id.value.contains '.' then id.depth == 0
  else true

def depthEqualsDotCount (id : IssueId) : Bool :=
  let dotCount := id.value.toList.filter (· == '.') |>.length
  id.depth == dotCount

def rootIdHasNoDots (id : IssueId) : Bool :=
  !id.rootId.value.contains '.'

def parentIdDepthCorrect (id : IssueId) : Bool :=
  match id.parentId? with
  | some parent => parent.depth + 1 == id.depth
  | none => id.depth == 0

def nextChildNumberEmpty (parentId : IssueId) : Bool :=
  nextChildNumber parentId [] == 1

def childIdFormat (parentId : IssueId) (childNum : Nat) : Bool :=
  match generateChildId parentId (childNum + 1) with
  | .ok childId => childId.value.startsWith parentId.value
  | .error _ => parentId.depth >= IssueId.maxDepth

def collisionHandled (title desc : String) (ts : Timestamp) : Bool :=
  let hash := generateFullHash title desc ts
  let id1 := allocateIdFromHash [] hash
  let id2 := allocateIdFromHash [id1] hash
  id1 != id2 || id2.length > id1.length

def allocatorProducesUnique (title desc : String) (ts : Timestamp) : Bool :=
  let hash := generateFullHash title desc ts
  let id1 := allocateIdFromHash [] hash
  let id2 := allocateIdFromHash [id1] hash
  ![id1].contains id2

def generatorAccumulates (title desc : String) (ts : Timestamp) : Bool :=
  let state : IdGenerator.State := {}
  let (id1, state1) := IdGenerator.generateId state title desc ts
  let (id2, state2) := IdGenerator.generateId state1 (title ++ "2") desc ts
  state2.existingIds.length == 2 &&
  state2.existingIds.contains id2.value &&
  state2.existingIds.contains id1.value

-- ============================================================================
-- ID Format Invariants
-- ============================================================================

-- All generated IDs start with "bd-" prefix
#eval Testable.check (∀ title desc : String, ∀ ts : Timestamp, idStartsWithPrefix title desc ts)
  (cfg := { numInst := 100, quiet := true })

-- Generated IDs have at least 6 hex characters after prefix
#eval Testable.check (∀ title desc : String, ∀ ts : Timestamp, idHasMinLength title desc ts)
  (cfg := { numInst := 100, quiet := true })

-- Hash is deterministic: same input produces same output
#eval Testable.check (∀ title desc : String, ∀ ts : Timestamp, hashIsDeterministic title desc ts)
  (cfg := { numInst := 100, quiet := true })

-- ============================================================================
-- ID Hierarchy Tests
-- ============================================================================

-- Root IDs have depth 0
#eval Testable.check (∀ id : IssueId, rootIdHasDepthZero id)
  (cfg := { numInst := 100, quiet := true })

-- Depth equals number of dots in ID
#eval Testable.check (∀ id : IssueId, depthEqualsDotCount id)
  (cfg := { numInst := 50, quiet := true })

-- Root ID extraction strips all suffixes
#eval Testable.check (∀ id : IssueId, rootIdHasNoDots id)
  (cfg := { numInst := 100, quiet := true })

-- Parent ID has depth one less than child
#eval Testable.check (∀ id : IssueId, parentIdDepthCorrect id)
  (cfg := { numInst := 50, quiet := true })

-- ============================================================================
-- Child ID Generation Tests
-- ============================================================================

-- Next child number with empty list is 1
#eval Testable.check (∀ parentId : IssueId, nextChildNumberEmpty parentId)
  (cfg := { numInst := 50, quiet := true })

-- Generated child ID has correct format
#eval Testable.check (∀ parentId : IssueId, ∀ childNum : Nat, childIdFormat parentId childNum)
  (cfg := { numInst := 50, quiet := true })

-- ============================================================================
-- Collision Handling Tests
-- ============================================================================

-- With existing ID, allocator extends length to avoid collision
#eval Testable.check (∀ title desc : String, ∀ ts : Timestamp, collisionHandled title desc ts)
  (cfg := { numInst := 50, quiet := true })

-- Allocator always produces unique IDs
#eval Testable.check (∀ title desc : String, ∀ ts : Timestamp, allocatorProducesUnique title desc ts)
  (cfg := { numInst := 50, quiet := true })

-- ============================================================================
-- IdGenerator State Tests
-- ============================================================================

-- IdGenerator accumulates IDs in state
#eval Testable.check (∀ title desc : String, ∀ ts : Timestamp, generatorAccumulates title desc ts)
  (cfg := { numInst := 30, quiet := true })
