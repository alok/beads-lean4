/-
Validated Types with Compile-Time Guarantees
Uses Lean's type system to enforce invariants at compile time
-/

namespace Beads

/-- Helper: check if haystack contains needle as substring -/
def hasSubstr (haystack needle : String) : Bool :=
  needle.isEmpty || (haystack.splitOn needle).length > 1

/-- A non-empty string with proof of non-emptiness -/
structure NonEmptyString where
  val : String
  nonEmpty : val.length > 0
  deriving Repr

namespace NonEmptyString

/-- Try to create a NonEmptyString, returning none if empty -/
def ofString? (s : String) : Option NonEmptyString :=
  if h : s.length > 0 then some ⟨s, h⟩ else none

/-- Create from string with proof obligation -/
def ofString (s : String) (h : s.length > 0 := by decide) : NonEmptyString := ⟨s, h⟩

instance : ToString NonEmptyString := ⟨fun s => s.val⟩
instance : BEq NonEmptyString := ⟨fun a b => a.val == b.val⟩
instance : DecidableEq NonEmptyString := fun a b =>
  if h : a.val = b.val then isTrue (by cases a; cases b; simp_all)
  else isFalse (by intro h'; cases h'; exact h rfl)

/-- Length is always positive -/
theorem length_pos (s : NonEmptyString) : s.val.length > 0 := s.nonEmpty

end NonEmptyString

/-- A bounded string with maximum length -/
structure BoundedString (maxLen : Nat) where
  val : String
  bounded : val.length ≤ maxLen
  deriving Repr

namespace BoundedString

def ofString? (s : String) (maxLen : Nat) : Option (BoundedString maxLen) :=
  if h : s.length ≤ maxLen then some ⟨s, h⟩ else none

instance : ToString (BoundedString n) := ⟨fun s => s.val⟩
instance : BEq (BoundedString n) := ⟨fun a b => a.val == b.val⟩

end BoundedString

/-- A title: non-empty and bounded to 500 chars (runtime checked) -/
structure Title where
  val : String
  nonEmpty : val.length > 0
  bounded : val.length ≤ 500
  deriving Repr

namespace Title

def ofString? (s : String) : Option Title :=
  if h1 : s.length > 0 then
    if h2 : s.length ≤ 500 then some ⟨s, h1, h2⟩
    else none
  else none

/-- Safe truncating constructor using native_decide for proofs -/
def ofStringTruncate (s : String) : Title :=
  let trimmed := s.trim
  let truncated := if trimmed.length > 500 then trimmed.take 500 else trimmed
  let result := if truncated.isEmpty then "Untitled" else truncated
  -- Use native_decide for runtime proof (safe since we construct valid values)
  if h1 : result.length > 0 then
    if h2 : result.length ≤ 500 then ⟨result, h1, h2⟩
    else ⟨"Untitled", by native_decide, by native_decide⟩
  else ⟨"Untitled", by native_decide, by native_decide⟩

instance : ToString Title := ⟨fun t => t.val⟩
instance : BEq Title := ⟨fun a b => a.val == b.val⟩
instance : Inhabited Title := ⟨⟨"Untitled", by native_decide, by native_decide⟩⟩

end Title

/-- A validated label: non-empty, trimmed -/
structure ValidLabel where
  val : String
  nonEmpty : val.length > 0
  deriving Repr

namespace ValidLabel

def ofString? (s : String) : Option ValidLabel :=
  let trimmed := s.trim
  if h : trimmed.length > 0 then some ⟨trimmed, h⟩
  else none

instance : ToString ValidLabel := ⟨fun l => l.val⟩
instance : BEq ValidLabel := ⟨fun a b => a.val == b.val⟩
instance : DecidableEq ValidLabel := fun a b =>
  if h : a.val = b.val then isTrue (by cases a; cases b; simp_all)
  else isFalse (by intro h'; cases h'; exact h rfl)

end ValidLabel

/-- A non-empty list with proof -/
structure NonEmptyList (α : Type u) where
  head : α
  tail : List α
  deriving Repr

namespace NonEmptyList

def toList {α : Type u} (nel : NonEmptyList α) : List α := nel.head :: nel.tail

def singleton {α : Type u} (x : α) : NonEmptyList α := ⟨x, []⟩

def cons {α : Type u} (x : α) (nel : NonEmptyList α) : NonEmptyList α :=
  ⟨x, nel.head :: nel.tail⟩

def length {α : Type u} (nel : NonEmptyList α) : Nat := 1 + nel.tail.length

theorem length_pos {α : Type u} (nel : NonEmptyList α) : nel.length > 0 := by
  simp [length]; omega

def fromList? {α : Type u} (xs : List α) : Option (NonEmptyList α) :=
  match xs with
  | [] => none
  | x :: xs => some ⟨x, xs⟩

def map {α β : Type u} (f : α → β) (nel : NonEmptyList α) : NonEmptyList β :=
  ⟨f nel.head, nel.tail.map f⟩

instance {α : Type u} [ToString α] : ToString (NonEmptyList α) :=
  ⟨fun nel => toString nel.toList⟩

end NonEmptyList

/-- A valid issue ID: non-empty -/
structure ValidIssueId where
  value : String
  nonEmpty : value.length > 0
  deriving Repr

namespace ValidIssueId

/-- Standard prefix for issue IDs -/
def idPrefix : String := "bd-"

def ofString? (s : String) : Option ValidIssueId :=
  if h : s.length > 0 then
    if s.startsWith "bd-" || hasSubstr s "." then some ⟨s, h⟩
    else none
  else none

/-- Check if this is a hierarchical (child) ID -/
def isHierarchical (id : ValidIssueId) : Bool :=
  hasSubstr id.value "."

/-- Get the depth (number of dots) -/
def depth (id : ValidIssueId) : Nat :=
  (id.value.toList.filter (· == '.')).length

instance : ToString ValidIssueId := ⟨fun id => id.value⟩
instance : BEq ValidIssueId := ⟨fun a b => a.value == b.value⟩
instance : Hashable ValidIssueId := ⟨fun id => hash id.value⟩

end ValidIssueId

/-- Result type alias for operations that can fail with a message -/
abbrev BeadsResult (α : Type u) := Except String α

/-- Timestamp with validity -/
structure ValidTimestamp where
  millis : Nat
  deriving Repr, DecidableEq, Hashable

namespace ValidTimestamp

def zero : ValidTimestamp := ⟨0⟩

def now : IO ValidTimestamp := do
  let t ← IO.monoMsNow
  pure ⟨t⟩

def fromNat (n : Nat) : ValidTimestamp := ⟨n⟩

def toNat (ts : ValidTimestamp) : Nat := ts.millis

instance : ToString ValidTimestamp := ⟨fun ts => toString ts.millis⟩
instance : Inhabited ValidTimestamp := ⟨zero⟩
instance : Ord ValidTimestamp := ⟨fun a b => compare a.millis b.millis⟩
instance : BEq ValidTimestamp := ⟨fun a b => a.millis == b.millis⟩

/-- A timestamp is before another -/
def before (a b : ValidTimestamp) : Bool := a.millis < b.millis

/-- Difference in milliseconds -/
def diff (a b : ValidTimestamp) : Int := (a.millis : Int) - (b.millis : Int)

end ValidTimestamp

/-- Issue invariant: if closed, must have closedAt -/
structure ClosedIssueInvariant where
  status : String
  closedAt : Option Nat
  valid : status = "closed" → closedAt.isSome

/-- Proof helper: construct valid closed state -/
def closedWithTimestamp (ts : Nat) : ClosedIssueInvariant :=
  ⟨"closed", some ts, fun _ => rfl⟩

/-- Proof helper: construct valid open state -/
def openState : ClosedIssueInvariant :=
  ⟨"open", none, fun h => by contradiction⟩

/-- Priority as a bounded natural number (0-4) -/
abbrev ValidPriority := Fin 5

namespace ValidPriority

def default : ValidPriority := ⟨2, by decide⟩
def critical : ValidPriority := ⟨0, by decide⟩
def high : ValidPriority := ⟨1, by decide⟩
def medium : ValidPriority := ⟨2, by decide⟩
def low : ValidPriority := ⟨3, by decide⟩
def lowest : ValidPriority := ⟨4, by decide⟩

def toString (p : ValidPriority) : String := s!"P{p.val}"

def fromNat? (n : Nat) : Option ValidPriority :=
  if h : n < 5 then some ⟨n, h⟩ else none

instance : ToString ValidPriority := ⟨toString⟩
instance : Inhabited ValidPriority := ⟨default⟩

/-- Compare priorities (lower value = higher priority) -/
def isHigherThan (a b : ValidPriority) : Bool := a.val < b.val

end ValidPriority

end Beads
