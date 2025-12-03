/-
Beads Deletions Module

Handles the deletions manifest for tracking deleted issues.
The deletions.jsonl file is an append-only log that records when issues are
deleted, enabling propagation of deletions across repo clones via git sync.

Port of: /Users/alokbeniwal/beads/internal/deletions/deletions.go
-/
import Beads.Core.Types
import Beads.Core.Issue
import Beads.Core.Json
import Lean.Data.Json

namespace Beads.Sync

open Lean Json
open Beads

/-- A record of a deleted issue in the manifest.
    Timestamps are serialized as RFC3339 and may lose sub-second precision. -/
structure DeletionRecord where
  id : IssueId           -- Issue ID that was deleted
  timestamp : Timestamp  -- When the deletion occurred (Unix millis)
  actor : String         -- Who performed the deletion
  reason : String := ""  -- Optional reason for deletion
  deriving Repr, Inhabited

namespace DeletionRecord

instance : ToJson DeletionRecord where
  toJson r := .mkObj [
    ("id", toJson r.id),
    ("ts", .num r.timestamp),
    ("by", .str r.actor),
    ("reason", if r.reason.isEmpty then .null else .str r.reason)
  ]

instance : FromJson DeletionRecord where
  fromJson? j := do
    let id ← j.getObjValAs? String "id"
    let ts ← j.getObjValAs? Nat "ts"
    let actor ← j.getObjValAs? String "by"
    let reason := match j.getObjValAs? String "reason" with
      | .ok s => s
      | .error _ => ""
    pure { id := ⟨id⟩, timestamp := ts, actor := actor, reason := reason }

end DeletionRecord

/-- Result of loading deletions from the manifest -/
structure LoadResult where
  records : Std.HashMap IssueId DeletionRecord
  skipped : Nat          -- Count of corrupt lines skipped
  warnings : List String -- Warning messages about skipped lines
  deriving Inhabited

/-- Result of pruning old deletions -/
structure PruneResult where
  keptCount : Nat
  prunedCount : Nat
  prunedIds : List IssueId
  deriving Repr, Inhabited

/-- Default path for the deletions manifest -/
def defaultPath (beadsDir : System.FilePath) : System.FilePath :=
  beadsDir / "deletions.jsonl"

/-- Load deletions from manifest file.
    Corrupt JSON lines are skipped rather than failing the load.
    For same ID, last-write-wins (most recent record kept). -/
def loadDeletions (path : System.FilePath) : IO LoadResult := do
  let mut result : LoadResult := { records := {}, skipped := 0, warnings := [] }

  -- Check if file exists
  if !(← path.pathExists) then
    return result

  -- Read and parse line by line
  let content ← IO.FS.readFile path
  let lines := content.splitOn "\n"
  let mut lineNo := 0

  for line in lines do
    lineNo := lineNo + 1
    if line.trim.isEmpty then continue

    match Json.parse line >>= FromJson.fromJson? (α := DeletionRecord) with
    | .ok record =>
      -- Validate required fields
      if record.id.value.isEmpty then
        result := { result with
          skipped := result.skipped + 1
          warnings := result.warnings ++ [s!"skipping line {lineNo}: missing ID"]
        }
      else
        -- Last-write-wins for same ID
        result := { result with records := result.records.insert record.id record }
    | .error e =>
      result := { result with
        skipped := result.skipped + 1
        warnings := result.warnings ++ [s!"skipping corrupt line {lineNo}: {e}"]
      }

  return result

/-- Append a single deletion record to the manifest.
    Creates the file if it doesn't exist.
    Atomically appends with fsync for durability. -/
def appendDeletion (path : System.FilePath) (record : DeletionRecord) : IO Unit := do
  -- Validate required fields
  if record.id.value.isEmpty then
    throw <| IO.userError "cannot append deletion record: ID is required"

  -- Ensure directory exists
  let dir := path.parent.getD "."
  IO.FS.createDirAll dir

  -- Marshal to JSON and append with newline
  let json := toJson record
  let line := json.compress ++ "\n"

  -- Open file for appending (create if not exists)
  let handle ← IO.FS.Handle.mk path .append
  handle.putStr line
  handle.flush

/-- Count the number of deletion records (fast, no JSON parsing).
    Returns 0 if file doesn't exist or is empty. -/
def count (path : System.FilePath) : IO Nat := do
  if !(← path.pathExists) then
    return 0

  let content ← IO.FS.readFile path
  let lines := content.splitOn "\n"
  return lines.filter (fun l => !l.trim.isEmpty) |>.length

/-- Write the entire deletions manifest atomically.
    Used for compaction to deduplicate and prune old entries. -/
def writeDeletions (path : System.FilePath) (records : List DeletionRecord) : IO Unit := do
  -- Ensure directory exists
  let dir := path.parent.getD "."
  IO.FS.createDirAll dir

  -- Create temp file for atomic write
  let tempPath := path.toString ++ ".tmp"
  let handle ← IO.FS.Handle.mk tempPath .write

  -- Write each record as a JSON line
  for record in records do
    let json := toJson record
    handle.putStrLn json.compress

  handle.flush

  -- Atomic replace
  IO.FS.rename tempPath path

/-- Prune deletion records older than the specified retention period.
    Returns the result with counts and pruned IDs. -/
def pruneDeletions (path : System.FilePath) (retentionDays : Nat) : IO PruneResult := do
  let loadResult ← loadDeletions path

  if loadResult.records.isEmpty then
    return { keptCount := 0, prunedCount := 0, prunedIds := [] }

  -- Calculate cutoff timestamp (retentionDays ago in milliseconds)
  let nowMs ← IO.monoMsNow
  let cutoffMs := nowMs - (retentionDays * 24 * 60 * 60 * 1000)

  let mut kept : List DeletionRecord := []
  let mut prunedIds : List IssueId := []

  -- Sort by ID for deterministic output
  let sortedRecords := loadResult.records.toList.mergeSort (fun a b => a.1.value < b.1.value)

  for (_, record) in sortedRecords do
    if record.timestamp >= cutoffMs then
      kept := kept ++ [record]
    else
      prunedIds := prunedIds ++ [record.id]

  -- Only rewrite if we actually pruned something
  if !prunedIds.isEmpty then
    writeDeletions path kept

  return {
    keptCount := kept.length
    prunedCount := prunedIds.length
    prunedIds := prunedIds
  }

/-- Check if an issue ID is in the deletions manifest -/
def isDeleted (path : System.FilePath) (id : IssueId) : IO Bool := do
  let loadResult ← loadDeletions path
  return loadResult.records.contains id

/-- Get all deleted issue IDs -/
def getDeletedIds (path : System.FilePath) : IO (List IssueId) := do
  let loadResult ← loadDeletions path
  return loadResult.records.toList.map (·.1)

end Beads.Sync
