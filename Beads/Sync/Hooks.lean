/-
Beads Hooks System

Pre/post hooks for various beads operations.
Hooks are shell scripts stored in .beads/hooks/
-/
import Lean.Data.Json
import Beads.Core

namespace Beads.Sync.Hooks

open Lean Json
open Beads

/-- Hook trigger points -/
inductive HookPoint where
  | preCreate
  | postCreate
  | preClose
  | postClose
  | preSync
  | postSync
  | preDelete
  | postDelete
  deriving Repr, Inhabited, BEq

def HookPoint.toString : HookPoint → String
  | .preCreate => "pre-create"
  | .postCreate => "post-create"
  | .preClose => "pre-close"
  | .postClose => "post-close"
  | .preSync => "pre-sync"
  | .postSync => "post-sync"
  | .preDelete => "pre-delete"
  | .postDelete => "post-delete"

def HookPoint.fromString? : String → Option HookPoint
  | "pre-create" => some .preCreate
  | "post-create" => some .postCreate
  | "pre-close" => some .preClose
  | "post-close" => some .postClose
  | "pre-sync" => some .preSync
  | "post-sync" => some .postSync
  | "pre-delete" => some .preDelete
  | "post-delete" => some .postDelete
  | _ => none

def HookPoint.all : List HookPoint :=
  [.preCreate, .postCreate, .preClose, .postClose, .preSync, .postSync, .preDelete, .postDelete]

/-- Hook execution result -/
structure HookResult where
  success : Bool
  output : String
  exitCode : UInt32
  deriving Repr, Inhabited

instance : ToJson HookResult where
  toJson r := Json.mkObj [
    ("success", Json.bool r.success),
    ("output", Json.str r.output),
    ("exit_code", Json.num r.exitCode.toNat)
  ]

/-- Hook manager -/
structure HookManager where
  hooksDir : System.FilePath
  deriving Inhabited

/-- Create hook manager for a beads directory -/
def HookManager.forDirectory (beadsDir : System.FilePath) : HookManager :=
  { hooksDir := beadsDir / "hooks" }

/-- Ensure hooks directory exists -/
def HookManager.ensureDir (hm : HookManager) : IO Unit := do
  if !(← hm.hooksDir.pathExists) then
    IO.FS.createDirAll hm.hooksDir

/-- Get the path for a hook -/
def HookManager.hookPath (hm : HookManager) (point : HookPoint) : System.FilePath :=
  hm.hooksDir / point.toString

/-- Check if a hook exists and is executable -/
def HookManager.exists (hm : HookManager) (point : HookPoint) : IO Bool := do
  let path := hm.hookPath point
  if !(← path.pathExists) then
    return false
  -- Check if executable (simplified - just check if file exists)
  return true

/-- List all installed hooks -/
def HookManager.list (hm : HookManager) : IO (List HookPoint) := do
  let mut installed : List HookPoint := []
  for point in HookPoint.all do
    if ← hm.exists point then
      installed := point :: installed
  return installed.reverse

/-- Run a hook with environment variables -/
def HookManager.run (hm : HookManager) (point : HookPoint) (env : List (String × String)) : IO HookResult := do
  let path := hm.hookPath point

  if !(← path.pathExists) then
    return { success := true, output := "", exitCode := 0 }

  -- Build environment array
  let envArr : Array (String × Option String) := env.toArray.map fun (k, v) => (k, some v)

  let result ← IO.Process.output {
    cmd := "/bin/sh"
    args := #["-c", path.toString]
    env := envArr
  }

  return {
    success := result.exitCode == 0
    output := result.stdout ++ result.stderr
    exitCode := result.exitCode
  }

/-- Install a hook from content -/
def HookManager.install (hm : HookManager) (point : HookPoint) (content : String) : IO Unit := do
  hm.ensureDir
  let path := hm.hookPath point
  IO.FS.writeFile path content
  -- Make executable
  let _ ← IO.Process.run { cmd := "chmod", args := #["+x", path.toString] }

/-- Remove a hook -/
def HookManager.remove (hm : HookManager) (point : HookPoint) : IO Bool := do
  let path := hm.hookPath point
  if ← path.pathExists then
    IO.FS.removeFile path
    return true
  return false

/-- Read hook content -/
def HookManager.read (hm : HookManager) (point : HookPoint) : IO (Option String) := do
  let path := hm.hookPath point
  if ← path.pathExists then
    return some (← IO.FS.readFile path)
  return none

/-- Initialize hooks directory with sample hooks -/
def HookManager.initSamples (hm : HookManager) : IO Unit := do
  hm.ensureDir

  -- Sample post-create hook
  let postCreate := "#!/bin/sh\n# Post-create hook\n# Available env: BEADS_ISSUE_ID, BEADS_ISSUE_TITLE, BEADS_ISSUE_TYPE\necho \"Created issue: $BEADS_ISSUE_ID\"\n"
  IO.FS.writeFile (hm.hooksDir / "post-create.sample") postCreate

  -- Sample pre-sync hook
  let preSync := "#!/bin/sh\n# Pre-sync hook\n# Runs before syncing with remote\n# Exit non-zero to abort sync\n"
  IO.FS.writeFile (hm.hooksDir / "pre-sync.sample") preSync

  -- Sample post-sync hook
  let postSync := "#!/bin/sh\n# Post-sync hook\n# Runs after successful sync\n# Available env: BEADS_SYNC_RESULT\n"
  IO.FS.writeFile (hm.hooksDir / "post-sync.sample") postSync

end Beads.Sync.Hooks
