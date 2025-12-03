/-
Beads Import Module

Imports issues from JSONL format into storage.
Handles sanitization (removing deleted issues) and tracking
created/updated/skipped counts.

Port of: /Users/alokbeniwal/beads/cmd/bd/sync.go importFromJSONL function
-/
import Beads.Core.Types
import Beads.Core.Issue
import Beads.Core.Json
import Beads.Sync.Deletions
import Beads.Sync.Export
import Lean.Data.Json

namespace Beads.Sync

open Lean Json
open Beads

/-- Result of an import operation -/
structure ImportResult where
  created : Nat          -- New issues imported
  updated : Nat          -- Existing issues updated
  skipped : Nat          -- Issues skipped (e.g., deleted)
  errors : List String   -- Parse/validation errors
  deriving Repr, Inhabited

/-- Result of sanitizing JSONL with deletions manifest -/
structure SanitizeResult where
  removedCount : Nat
  removedIds : List IssueId
  keptCount : Nat
  deriving Repr, Inhabited

/-- Load and parse issues from JSONL file -/
def loadIssuesFromJSONL (path : System.FilePath) : IO (List Issue × List String) := do
  if !(← path.pathExists) then
    return ([], [s!"file not found: {path}"])

  let content ← IO.FS.readFile path
  return parseJSONL content

/-- Sanitize JSONL by removing deleted issues before import.
    This prevents zombie resurrection when git's 3-way merge
    re-adds deleted issues to JSONL. -/
def sanitizeWithDeletions (jsonlPath : System.FilePath) : IO SanitizeResult := do
  let beadsDir := jsonlPath.parent.getD "."
  let deletionsPath := defaultPath beadsDir

  -- Load deletions manifest
  let loadResult ← loadDeletions deletionsPath
  if loadResult.records.isEmpty then
    let count ← countIssuesInJSONL jsonlPath
    return { removedCount := 0, removedIds := [], keptCount := count }

  -- Load and parse JSONL
  let (issues, _) ← loadIssuesFromJSONL jsonlPath
  let mut kept : List Issue := []
  let mut removed : List IssueId := []

  for issue in issues do
    if loadResult.records.contains issue.id then
      removed := removed ++ [issue.id]
    else
      kept := kept ++ [issue]

  -- If nothing removed, we're done
  if removed.isEmpty then
    return { removedCount := 0, removedIds := [], keptCount := kept.length }

  -- Rewrite sanitized JSONL
  let _ ← exportToJSONL kept jsonlPath

  return {
    removedCount := removed.length
    removedIds := removed
    keptCount := kept.length
  }

/-- Import issues from JSONL into a mutable issue list.
    Returns import statistics. -/
def importIssues (existing : List Issue) (jsonlPath : System.FilePath) : IO (List Issue × ImportResult) := do
  let (newIssues, errors) ← loadIssuesFromJSONL jsonlPath

  if !errors.isEmpty then
    return (existing, { created := 0, updated := 0, skipped := 0, errors := errors })

  -- Build map of existing issues
  let existingMap : Std.HashMap IssueId Issue := existing.foldl (fun m i => m.insert i.id i) {}

  let mut result : List Issue := existing
  let mut created := 0
  let mut updated := 0
  let mut skipped := 0

  for issue in newIssues do
    match existingMap.get? issue.id with
    | none =>
      -- New issue
      result := result ++ [issue]
      created := created + 1
    | some existingIssue =>
      -- Update if newer
      if issue.updatedAt >= existingIssue.updatedAt then
        result := result.filter (fun i => i.id != issue.id) ++ [issue]
        updated := updated + 1
      else
        skipped := skipped + 1

  return (result, {
    created := created
    updated := updated
    skipped := skipped
    errors := []
  })

/-- Full import with sanitization -/
def importWithSanitization (existing : List Issue) (jsonlPath : System.FilePath) : IO (List Issue × ImportResult × SanitizeResult) := do
  -- First sanitize (remove deleted issues from JSONL)
  let sanitizeResult ← sanitizeWithDeletions jsonlPath

  -- Then import
  let (result, importResult) ← importIssues existing jsonlPath

  return (result, importResult, sanitizeResult)

/-- Validate issue before import -/
def validateIssue (issue : Issue) : Except String Unit := do
  if issue.title.isEmpty then
    throw "title is required"
  if issue.title.length > 500 then
    throw s!"title must be 500 characters or less (got {issue.title.length})"
  -- closedAt invariant
  match issue.status, issue.closedAt with
  | .closed, none => throw "closed issues must have closed_at timestamp"
  | s, some _ => if s != .closed then throw "non-closed issues cannot have closed_at timestamp" else pure ()
  | _, _ => pure ()

/-- Import with validation -/
def importWithValidation (existing : List Issue) (jsonlPath : System.FilePath) : IO (List Issue × ImportResult) := do
  let (newIssues, parseErrors) ← loadIssuesFromJSONL jsonlPath

  if !parseErrors.isEmpty then
    return (existing, { created := 0, updated := 0, skipped := 0, errors := parseErrors })

  -- Validate all issues first
  let mut validationErrors : List String := []
  for issue in newIssues do
    match validateIssue issue with
    | .ok _ => pure ()
    | .error e => validationErrors := validationErrors ++ [s!"{issue.id.value}: {e}"]

  if !validationErrors.isEmpty then
    return (existing, { created := 0, updated := 0, skipped := 0, errors := validationErrors })

  -- All valid, proceed with import
  importIssues existing jsonlPath

end Beads.Sync
