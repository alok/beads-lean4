/-
Beads Export Module

Exports issues from storage to JSONL format for git sync.
Includes ZFC (Zero Filesystem Copies) safety checks to prevent
exporting stale database over populated JSONL.

Port of: /Users/alokbeniwal/beads/cmd/bd/sync.go exportToJSONL function
-/
import Beads.Core.Types
import Beads.Core.Issue
import Beads.Core.Json
import Lean.Data.Json

namespace Beads.Sync

open Lean Json
open Beads

/-- Result of an export operation -/
structure ExportResult where
  issueCount : Nat
  path : System.FilePath
  skipped : Bool        -- True if export was skipped (ZFC)
  reason : String := "" -- Reason if skipped
  deriving Repr, Inhabited

/-- ZFC (Zero Filesystem Copies) safety check result -/
inductive ZFCCheck where
  | safe : ZFCCheck
  | staleDB : Nat → Nat → ZFCCheck     -- DB count, JSONL count
  | emptyDB : Nat → ZFCCheck           -- JSONL count
  deriving Repr

/-- Perform ZFC safety check before export.
    Prevents exporting stale/empty DB over populated JSONL. -/
def checkZFCSafety (dbCount jsonlCount : Nat) : ZFCCheck :=
  -- Case 1: Empty DB over non-empty JSONL is always dangerous
  if dbCount == 0 && jsonlCount > 0 then
    .emptyDB jsonlCount
  -- Case 2: JSONL has significantly more issues than DB (>20% divergence)
  else if jsonlCount > dbCount && jsonlCount > 0 then
    let divergence := (jsonlCount - dbCount).toFloat / jsonlCount.toFloat
    if divergence > 0.2 then
      .staleDB dbCount jsonlCount
    else
      .safe
  else
    .safe

/-- Count issues in a JSONL file -/
def countIssuesInJSONL (path : System.FilePath) : IO Nat := do
  if !(← path.pathExists) then
    return 0
  let content ← IO.FS.readFile path
  let lines := content.splitOn "\n"
  return lines.filter (fun l => !l.trim.isEmpty) |>.length

/-- Export issues to JSONL file with atomic write.
    Issues are sorted by ID for deterministic output. -/
def exportToJSONL (issues : List Issue) (path : System.FilePath) : IO ExportResult := do
  -- Sort by ID for consistent output
  let sorted := issues.mergeSort (fun a b => a.id.value < b.id.value)

  -- Create temp file for atomic write
  let tempPath := path.toString ++ ".tmp"
  let handle ← IO.FS.Handle.mk tempPath .write

  -- Write each issue as JSON line
  for issue in sorted do
    let json := toJson issue
    handle.putStrLn json.compress

  handle.flush

  -- Atomic replace
  IO.FS.rename tempPath path

  return {
    issueCount := issues.length
    path := path
    skipped := false
  }

/-- Export with ZFC safety check -/
def exportWithZFCCheck (issues : List Issue) (path : System.FilePath) : IO ExportResult := do
  let jsonlCount ← countIssuesInJSONL path
  let dbCount := issues.length

  match checkZFCSafety dbCount jsonlCount with
  | .safe => exportToJSONL issues path
  | .emptyDB count =>
    return {
      issueCount := 0
      path := path
      skipped := true
      reason := s!"refusing to export empty database over non-empty JSONL (JSONL has {count} issues)"
    }
  | .staleDB db jsonl =>
    return {
      issueCount := 0
      path := path
      skipped := true
      reason := s!"stale DB detected: DB has {db} issues but JSONL has {jsonl} (>20% divergence)"
    }

/-- Parse issues from JSONL content -/
def parseJSONL (content : String) : List Issue × List String := Id.run do
  let lines := content.splitOn "\n"
  let mut issues : List Issue := []
  let mut errors : List String := []
  let mut lineNo := 0

  for line in lines do
    lineNo := lineNo + 1
    if line.trim.isEmpty then continue

    match Json.parse line >>= FromJson.fromJson? (α := Issue) with
    | .ok issue => issues := issues ++ [issue]
    | .error e => errors := errors ++ [s!"line {lineNo}: {e}"]

  (issues, errors)

/-- Compute a simple hash of JSONL content for staleness detection -/
def computeJSONLHash (path : System.FilePath) : IO String := do
  if !(← path.pathExists) then
    return ""
  let content ← IO.FS.readFile path
  -- Simple hash: count + length + first 100 chars
  let lines := content.splitOn "\n" |>.filter (fun l => !l.trim.isEmpty)
  let preview := content.take 100
  return s!"{lines.length}:{content.length}:{preview.hash}"

end Beads.Sync
