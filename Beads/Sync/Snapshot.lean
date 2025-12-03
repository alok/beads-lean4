/-
Beads Snapshot Module

Manages snapshots for 3-way merge during git sync.
Before pulling, we capture the current JSONL state as "left" snapshot.
After pull, we have:
- Base: Common ancestor (from git merge-base)
- Left: Our pre-pull state (captured snapshot)
- Right: Their post-pull state (after git pull)
-/
import Beads.Core.Types
import Beads.Core.Issue

namespace Beads.Sync

open Beads

/-- Snapshot file paths relative to the JSONL file -/
structure SnapshotPaths where
  jsonlPath : System.FilePath
  leftPath : System.FilePath   -- Pre-pull snapshot
  basePath : System.FilePath   -- Merge base (from git)
  deriving Repr, Inhabited

/-- Create snapshot paths from the main JSONL path -/
def snapshotPaths (jsonlPath : System.FilePath) : SnapshotPaths :=
  let dir := jsonlPath.parent.getD "."
  let base := jsonlPath.fileName.getD "issues.jsonl"
  let baseName := base.dropRight 6  -- Remove ".jsonl"
  {
    jsonlPath := jsonlPath
    leftPath := dir / (baseName ++ ".left.jsonl")
    basePath := dir / (baseName ++ ".base.jsonl")
  }

/-- Capture the current JSONL state as "left" snapshot (pre-pull).
    This is called before git pull to preserve our local state for 3-way merge. -/
def captureLeftSnapshot (jsonlPath : System.FilePath) : IO Unit := do
  let paths := snapshotPaths jsonlPath

  -- Check if source exists
  if !(← jsonlPath.pathExists) then
    -- No JSONL file yet - nothing to capture
    return

  -- Copy JSONL to left snapshot
  let content ← IO.FS.readFile jsonlPath
  IO.FS.writeFile paths.leftPath content

/-- Check if left snapshot exists -/
def hasLeftSnapshot (jsonlPath : System.FilePath) : IO Bool := do
  let paths := snapshotPaths jsonlPath
  paths.leftPath.pathExists

/-- Get the left snapshot content if it exists -/
def getLeftSnapshot (jsonlPath : System.FilePath) : IO (Option String) := do
  let paths := snapshotPaths jsonlPath
  if ← paths.leftPath.pathExists then
    some <$> IO.FS.readFile paths.leftPath
  else
    pure none

/-- Update the base snapshot after successful sync.
    The current JSONL becomes the new base for future syncs. -/
def updateBaseSnapshot (jsonlPath : System.FilePath) : IO Unit := do
  let paths := snapshotPaths jsonlPath

  if !(← jsonlPath.pathExists) then
    return

  let content ← IO.FS.readFile jsonlPath
  IO.FS.writeFile paths.basePath content

/-- Get the base snapshot content if it exists -/
def getBaseSnapshot (jsonlPath : System.FilePath) : IO (Option String) := do
  let paths := snapshotPaths jsonlPath
  if ← paths.basePath.pathExists then
    some <$> IO.FS.readFile paths.basePath
  else
    pure none

/-- Clean up temporary snapshot files after successful sync -/
def cleanupSnapshots (jsonlPath : System.FilePath) : IO Unit := do
  let paths := snapshotPaths jsonlPath

  -- Remove left snapshot (temporary, only needed during sync)
  if ← paths.leftPath.pathExists then
    IO.FS.removeFile paths.leftPath

  -- Note: base snapshot is kept for next sync

/-- Remove all snapshot files (for reset) -/
def removeAllSnapshots (jsonlPath : System.FilePath) : IO Unit := do
  let paths := snapshotPaths jsonlPath

  if ← paths.leftPath.pathExists then
    IO.FS.removeFile paths.leftPath

  if ← paths.basePath.pathExists then
    IO.FS.removeFile paths.basePath

/-- Information about current snapshot state -/
structure SnapshotInfo where
  hasLeft : Bool
  hasBase : Bool
  deriving Repr, Inhabited

/-- Get information about current snapshots -/
def getSnapshotInfo (jsonlPath : System.FilePath) : IO SnapshotInfo := do
  let paths := snapshotPaths jsonlPath
  let hasLeft ← paths.leftPath.pathExists
  let hasBase ← paths.basePath.pathExists
  return { hasLeft, hasBase }

end Beads.Sync
