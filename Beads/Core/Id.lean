/-
Beads ID Generation
Port of internal/types/id_generator.go

Hash-based collision-resistant ID generation.
Format: prefix-{6-8-char-hex} with progressive extension on collision
Examples: bd-a3f2dd (6), bd-a3f2dda (7), bd-a3f2dda8 (8)
-/
import Beads.Core.Issue

namespace Beads

/-- Simple hash function for ID generation.
    In production, this would use SHA256. For now, we use a simpler approach. -/
private def simpleHash (s : String) : UInt64 :=
  s.foldl (fun h c => h * 31 + c.toNat.toUInt64) 5381

/-- Convert UInt64 to hex string -/
private def toHex (n : UInt64) : String :=
  let hexDigits := "0123456789abcdef"
  go hexDigits n [] 20  -- 20 iterations is enough for 64-bit
where
  go (hexDigits : String) (n : UInt64) (acc : List Char) (fuel : Nat) : String :=
    if h : fuel == 0 then String.ofList acc
    else if n == 0 then
      if acc.isEmpty then "0" else String.ofList acc
    else
      let digit := (n % 16).toNat
      let c := hexDigits.toList.getD digit '0'
      go hexDigits (n / 16) (c :: acc) (fuel - 1)
  termination_by fuel
  decreasing_by simp_all; omega

/-- Pad a hex string to at least n characters -/
private def padHex (s : String) (n : Nat) : String :=
  let padding := String.ofList (List.replicate (n - min n s.length) '0')
  padding ++ s

/-- Generate a full hash from issue content.
    Returns a 16-char hex string for progressive length selection. -/
def generateFullHash (title description : String) (created : Timestamp) (workspaceId : String := "") : String :=
  let input := s!"{title}\x00{description}\x00{created}\x00{workspaceId}"
  let hash := simpleHash input
  let hexHash := padHex (toHex hash) 16
  hexHash

/-- Default ID prefix -/
def defaultPrefix : String := "bd"

/-- Allocate an ID, progressively extending length on collision.
    existingIds: Set of already-used IDs
    fullHash: The full hash string (at least 16 chars)
    idPrefix: ID prefix (default "bd")
    Returns: A unique ID string like "bd-a3f2dd" -/
def allocateIdFromHash (existingIds : List String) (fullHash : String) (idPrefix : String := defaultPrefix) : String :=
  let existingSet := existingIds.toArray
  go existingSet fullHash idPrefix 6 12
where
  go (existingSet : Array String) (fullHash idPrefix : String) (len fuel : Nat) : String :=
    if h : fuel == 0 then s!"{idPrefix}-{fullHash}"
    else if len > 16 then
      -- Fallback to full hash if all shorter versions collide (very unlikely)
      s!"{idPrefix}-{fullHash}"
    else
      let candidate := s!"{idPrefix}-{fullHash.take len}"
      if existingSet.contains candidate then
        go existingSet fullHash idPrefix (len + 1) (fuel - 1)
      else
        candidate
  termination_by fuel
  decreasing_by simp_all; omega

/-- Generate a child ID.
    Format: parent.N (e.g., "bd-af78e9a2.1", "bd-af78e9a2.1.2")
    Max depth: 3 levels -/
def generateChildId (parentId : IssueId) (childNumber : Nat) : Except String IssueId :=
  let currentDepth := parentId.depth
  if currentDepth >= IssueId.maxDepth then
    .error s!"Maximum hierarchy depth ({IssueId.maxDepth}) exceeded"
  else
    .ok ⟨s!"{parentId.value}.{childNumber}"⟩

/-- Get maximum of a list of Nats -/
private def listMax (xs : List Nat) : Option Nat :=
  xs.foldl (fun acc x => match acc with
    | none => some x
    | some m => some (max m x)) none

/-- Find the next available child number for a parent -/
def nextChildNumber (parentId : IssueId) (existingChildren : List IssueId) : Nat :=
  let childNums := existingChildren.filterMap fun id =>
    let parts := id.value.splitOn "."
    let parentParts := parentId.value.splitOn "."
    -- Check if this is a direct child of parent
    if parts.length == parentParts.length + 1 then
      -- Extract the child number (last part)
      parts.getLast?.bind (·.toNat?)
    else
      none
  match listMax childNums with
  | some max => max + 1
  | none => 1

namespace IdGenerator

/-- State for ID generation within a workspace -/
structure State where
  existingIds : List String := []
  workspaceId : String := ""
  deriving Repr, Inhabited

/-- Generate a new unique ID for an issue -/
def generateId (state : State) (title description : String) (created : Timestamp) : IssueId × State :=
  let fullHash := generateFullHash title description created state.workspaceId
  let newId := allocateIdFromHash state.existingIds fullHash
  let newState := { state with existingIds := newId :: state.existingIds }
  (⟨newId⟩, newState)

/-- Generate a child ID under a parent -/
def generateChildIdFor (state : State) (parentId : IssueId) : Except String (IssueId × State) := do
  let existingChildren := state.existingIds.filterMap fun s =>
    if s.startsWith parentId.value && s.length > parentId.value.length then
      -- Check if the character after parent is a dot
      let suffix := s.drop parentId.value.length
      if suffix.startsWith "." then
        some ⟨s⟩
      else
        none
    else
      none
  let childNum := nextChildNumber parentId existingChildren
  let childId ← Beads.generateChildId parentId childNum
  let newState := { state with existingIds := childId.value :: state.existingIds }
  .ok (childId, newState)

end IdGenerator

end Beads
