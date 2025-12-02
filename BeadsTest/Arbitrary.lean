/-
Custom Arbitrary and Shrinkable instances for Beads types
Used by Plausible for property-based testing
-/
import Beads
import Plausible.Arbitrary
import Plausible.Sampleable
import Plausible.DeriveArbitrary

open Plausible
open Beads

-- Derive Arbitrary for simple enum types
deriving instance Arbitrary for Status
deriving instance Arbitrary for IssueType
deriving instance Arbitrary for DependencyType
deriving instance Arbitrary for EventType
deriving instance Arbitrary for SortPolicy

-- Shrinkable instances for simple enums (no meaningful shrinking)
instance : Shrinkable Status where shrink _ := []
instance : Shrinkable IssueType where shrink _ := []
instance : Shrinkable DependencyType where shrink _ := []
instance : Shrinkable EventType where shrink _ := []
instance : Shrinkable SortPolicy where shrink _ := []

-- Priority is Fin 5, which already has Arbitrary
-- Add Shrinkable for Priority (just use default - no meaningful shrinking for small enums)
instance : Shrinkable Priority where shrink _ := []

-- IssueId: Generate valid bd-XXXXXX format IDs
instance : Arbitrary IssueId where
  arbitrary := do
    let len ← Gen.choose Nat 6 8 (by omega)
    let chars ← Gen.listOf (Arbitrary.arbitrary : Gen Char)
    let hexChars := chars.map fun c =>
      let n := c.toNat % 16
      if n < 10 then Char.ofNat (n + 48)  -- '0'-'9'
      else Char.ofNat (n - 10 + 97)       -- 'a'-'f'
    pure ⟨"bd-" ++ String.ofList (hexChars.take len.val)⟩

instance : Shrinkable IssueId where
  shrink id :=
    -- Shrink by removing characters from the hex part
    let s := id.value
    if s.startsWith "bd-" then
      let hex := s.drop 3
      if hex.length > 6 then
        [⟨"bd-" ++ hex.take (hex.length - 1)⟩]
      else
        []
    else
      []

-- Timestamp: Generate reasonable timestamps
instance : Arbitrary Timestamp where
  arbitrary := do
    -- Generate timestamps in a reasonable range (last ~10 years in ms)
    let base : Nat := 1600000000000  -- ~2020
    let offset ← Gen.choose Nat 0 400000000000 (by omega)  -- ~12 years
    pure (base + offset.val)

instance : Shrinkable Timestamp where
  shrink ts :=
    -- Shrink towards smaller timestamps
    if ts > 1600000000000 then
      [ts - 1000, ts / 2].filter (· ≥ 1600000000000)
    else
      []

-- Issue: Generate valid issues respecting invariants
instance : Arbitrary Issue where
  arbitrary := do
    let id ← Arbitrary.arbitrary
    let title ← do
      let len ← Gen.choose Nat 1 50 (by omega)
      let chars ← Gen.listOf (Arbitrary.arbitrary : Gen Char)
      -- Simple chars for readability
      let titleChars := chars.map fun c =>
        let n := c.toNat % 26
        Char.ofNat (n + 97)  -- lowercase letters
      pure (String.ofList (titleChars.take len.val))
    let description ← Arbitrary.arbitrary
    let status ← Arbitrary.arbitrary
    let priority ← Arbitrary.arbitrary
    let issueType ← Arbitrary.arbitrary
    let createdAt ← Arbitrary.arbitrary
    let updatedAt : Timestamp ← do
      -- updatedAt should be >= createdAt
      let offset ← Gen.choose Nat 0 10000 (by omega)
      pure (createdAt + offset.val)
    -- closedAt must exist iff status is closed
    let closedAt : Option Timestamp := match status with
      | .closed => some (updatedAt + 1)
      | _ => none
    pure {
      id
      title
      description
      status
      priority
      issueType
      createdAt
      updatedAt
      closedAt
      closeReason := if status == .closed then "test" else ""
    }

instance : Shrinkable Issue where
  shrink issue :=
    -- Shrink title (most useful for finding minimal counter-examples)
    let shrinkTitle := (Shrinkable.shrink issue.title).filterMap fun t =>
      if t.isEmpty then none else some { issue with title := t }
    -- Shrink priority towards default (P2)
    let shrinkPriority := (Shrinkable.shrink issue.priority).map fun p =>
      { issue with priority := p }
    shrinkTitle ++ shrinkPriority

-- Dependency: Generate valid dependencies
instance : Arbitrary Dependency where
  arbitrary := do
    let issueId ← Arbitrary.arbitrary
    let dependsOnId ← Arbitrary.arbitrary
    let depType ← Arbitrary.arbitrary
    let createdAt ← Arbitrary.arbitrary
    let createdBy := "test-user"
    pure { issueId, dependsOnId, depType, createdAt, createdBy }

instance : Shrinkable Dependency where
  shrink _ := []  -- Dependencies are simple, no meaningful shrinking

-- IssueFilter
instance : Arbitrary IssueFilter where
  arbitrary := do
    let status ← Arbitrary.arbitrary
    let priority ← Arbitrary.arbitrary
    let limit ← Gen.choose Nat 1 100 (by omega)
    pure { status, priority, limit := limit.val }

-- WorkFilter
instance : Arbitrary WorkFilter where
  arbitrary := do
    let priority ← Arbitrary.arbitrary
    let limit ← Gen.choose Nat 1 20 (by omega)
    let sortPolicy ← Arbitrary.arbitrary
    pure { priority, limit := limit.val, sortPolicy }

-- BlockedIssue
instance : Arbitrary BlockedIssue where
  arbitrary := do
    let issue ← Arbitrary.arbitrary
    let blockedByCount ← Gen.choose Nat 0 5 (by omega)
    let blockedBy ← Gen.listOf Arbitrary.arbitrary
    pure { issue, blockedByCount := blockedByCount.val, blockedBy := blockedBy.take blockedByCount.val }
