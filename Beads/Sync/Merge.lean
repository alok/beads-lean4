/-
Beads 3-Way Merge Module

Performs 3-way merge of JSONL issue files for git sync.

Key merge rules (from Go implementation):
1. Deletion wins - if issue removed in either branch, stays deleted
2. Closed wins - if status=closed in either side, stays closed
3. Max timestamp - updatedAt takes the maximum
4. Union deps - dependencies are merged and deduplicated

Port of: /Users/alokbeniwal/beads/internal/merge/merge.go
-/
import Beads.Core.Types
import Beads.Core.Issue
import Beads.Core.Json
import Lean.Data.Json

namespace Beads.Sync

open Lean Json
open Beads

/-- Result of merging a single issue -/
inductive MergeResult where
  | clean : Issue → MergeResult             -- No conflicts, merged cleanly
  | conflict : Issue → Issue → MergeResult  -- Both sides changed differently
  deriving Repr

/-- Output of 3-way merge -/
structure MergeOutput where
  merged : List Issue           -- Successfully merged issues
  conflicts : List MergeResult  -- Conflicts needing resolution
  deriving Repr, Inhabited

/-- Key for matching issues across versions -/
structure IssueKey where
  id : IssueId
  createdAt : Timestamp
  deriving BEq, Hashable, Repr

def issueToKey (i : Issue) : IssueKey := { id := i.id, createdAt := i.createdAt }

/-- Merge a single field with 3-way logic -/
def mergeField (base left right : String) : String :=
  if base == left && base != right then right
  else if base == right && base != left then left
  else left  -- Both same or both changed to same, take left

/-- Merge status with "closed wins" rule -/
def mergeStatus (base left right : Status) : Status :=
  -- RULE 1: closed always wins over open
  if left == .closed || right == .closed then .closed
  else mergeField base.toString left.toString right.toString |> Status.fromString? |>.getD left

/-- Merge timestamps - take the maximum -/
def mergeTimestamp (left right : Timestamp) : Timestamp :=
  max left right

/-- Merge optional timestamps - take max if both present -/
def mergeOptTimestamp (left right : Option Timestamp) : Option Timestamp :=
  match left, right with
  | some l, some r => some (max l r)
  | some l, none => some l
  | none, some r => some r
  | none, none => none

/-- Merge priorities -/
def mergePriority (base left right : Priority) : Priority :=
  if base == left && base != right then right
  else if base == right && base != left then left
  else left  -- Both same or both changed, take left

/-- Check if there's a real conflict (both sides changed differently) -/
def hasConflict (base left right : Issue) : Bool :=
  -- Title conflict
  (base.title != left.title && base.title != right.title && left.title != right.title) ||
  -- Description conflict
  (base.description != left.description && base.description != right.description && left.description != right.description) ||
  -- Notes conflict
  (base.notes != left.notes && base.notes != right.notes && left.notes != right.notes) ||
  -- Status conflict (but closed wins, so not a real conflict if one is closed)
  (left.status != .closed && right.status != .closed &&
   base.status != left.status && base.status != right.status && left.status != right.status) ||
  -- Priority conflict
  (base.priority != left.priority && base.priority != right.priority && left.priority != right.priority) ||
  -- Issue type conflict
  (base.issueType != left.issueType && base.issueType != right.issueType && left.issueType != right.issueType)

/-- Merge a single issue from all three versions -/
def mergeIssue (base left right : Issue) : MergeResult :=
  let merged : Issue := {
    id := base.id
    title := mergeField base.title left.title right.title
    description := mergeField base.description left.description right.description
    design := mergeField base.design left.design right.design
    acceptanceCriteria := mergeField base.acceptanceCriteria left.acceptanceCriteria right.acceptanceCriteria
    notes := mergeField base.notes left.notes right.notes
    status := mergeStatus base.status left.status right.status
    priority := mergePriority base.priority left.priority right.priority
    issueType := if base.issueType == left.issueType then right.issueType else left.issueType
    assignee := if base.assignee == left.assignee then right.assignee else left.assignee
    estimatedMinutes := if base.estimatedMinutes == left.estimatedMinutes then right.estimatedMinutes else left.estimatedMinutes
    createdAt := base.createdAt
    updatedAt := mergeTimestamp left.updatedAt right.updatedAt
    -- closedAt: only set if status is closed
    closedAt := if mergeStatus base.status left.status right.status == .closed
                then mergeOptTimestamp left.closedAt right.closedAt
                else none
    closeReason := mergeField base.closeReason left.closeReason right.closeReason
    externalRef := if base.externalRef == left.externalRef then right.externalRef else left.externalRef
    labels := (left.labels ++ right.labels).eraseDups
  }
  if hasConflict base left right then
    .conflict left right
  else
    .clean merged

/-- Check if two issues are equal (for comparing added-in-both case) -/
def issuesEqual (a b : Issue) : Bool :=
  a.id == b.id &&
  a.title == b.title &&
  a.description == b.description &&
  a.status == b.status &&
  a.priority == b.priority &&
  a.issueType == b.issueType

/-- Build a map from issue list -/
def buildIssueMap (issues : List Issue) : Std.HashMap IssueKey Issue :=
  issues.foldl (fun m i => m.insert (issueToKey i) i) {}

/-- Perform 3-way merge of issue lists

    base: Common ancestor
    left: Our version (local changes)
    right: Their version (remote changes)

    Returns merged issues and any conflicts. -/
def merge3Way (base left right : List Issue) : MergeOutput := Id.run do
  let baseMap := buildIssueMap base
  let leftMap := buildIssueMap left
  let rightMap := buildIssueMap right

  -- Collect all unique keys
  let allKeys := (baseMap.toList.map (·.1) ++ leftMap.toList.map (·.1) ++ rightMap.toList.map (·.1)).eraseDups

  let mut merged : List Issue := []
  let mut conflicts : List MergeResult := []

  for key in allKeys do
    let inBase := baseMap.get? key
    let inLeft := leftMap.get? key
    let inRight := rightMap.get? key

    match inBase, inLeft, inRight with
    -- All three present - perform merge
    | some b, some l, some r =>
      match mergeIssue b l r with
      | .clean i => merged := merged ++ [i]
      | c@(.conflict _ _) => conflicts := conflicts ++ [c]

    -- Added in both - check if identical
    | none, some l, some r =>
      if issuesEqual l r then
        merged := merged ++ [l]
      else
        conflicts := conflicts ++ [.conflict l r]

    -- Deleted in right - RULE: deletion wins over modification
    | some _, some _, none => pure ()  -- Deleted

    -- Deleted in left - RULE: deletion wins over modification
    | some _, none, some _ => pure ()  -- Deleted

    -- Added only in left
    | none, some l, none => merged := merged ++ [l]

    -- Added only in right
    | none, none, some r => merged := merged ++ [r]

    -- Other cases (shouldn't happen)
    | _, _, _ => pure ()

  return { merged, conflicts }

/-- Format a conflict as git-style markers -/
def formatConflict (result : MergeResult) : String :=
  match result with
  | .clean _ => ""  -- Not a conflict
  | .conflict left right =>
    let leftJson := toJson left |>.compress
    let rightJson := toJson right |>.compress
    s!"<<<<<<< left\n{leftJson}\n=======\n{rightJson}\n>>>>>>> right"

/-- Check if merge output has any conflicts -/
def MergeOutput.hasConflicts (output : MergeOutput) : Bool :=
  !output.conflicts.isEmpty

/-- Get all conflict issue IDs -/
def MergeOutput.conflictIds (output : MergeOutput) : List IssueId :=
  output.conflicts.filterMap fun r =>
    match r with
    | .clean _ => none
    | .conflict left _ => some left.id

end Beads.Sync
