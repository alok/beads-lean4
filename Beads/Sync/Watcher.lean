/-
Beads File Watcher

Simple polling-based file watcher that monitors .beads/ for changes
and can trigger auto-sync.
-/
import Lean.Data.Json
import Beads.Core
import Beads.Sync.Git

namespace Beads.Sync.Watcher

open Lean Json
open Beads

/-- Watcher configuration -/
structure WatcherConfig where
  beadsDir : System.FilePath
  pollIntervalMs : Nat := 5000    -- Poll every 5 seconds
  autoSync : Bool := false        -- Auto-sync on changes
  verbose : Bool := false
  deriving Repr, Inhabited

instance : ToJson WatcherConfig where
  toJson c := Json.mkObj [
    ("beads_dir", Json.str c.beadsDir.toString),
    ("poll_interval_ms", Json.num c.pollIntervalMs),
    ("auto_sync", Json.bool c.autoSync),
    ("verbose", Json.bool c.verbose)
  ]

/-- File state for change detection -/
structure FileState where
  path : System.FilePath
  modTime : Nat  -- Unix timestamp
  size : Nat
  deriving Repr, Inhabited, BEq

/-- Watcher state -/
structure WatcherState where
  config : WatcherConfig
  lastScan : Nat  -- Unix timestamp
  fileStates : List FileState
  changeCount : Nat := 0
  syncCount : Nat := 0
  deriving Repr, Inhabited

/-- Get file metadata using stat -/
def getFileState (path : System.FilePath) : IO (Option FileState) := do
  if !(← path.pathExists) then
    return none
  -- Use stat to get modification time
  let result ← IO.Process.output {
    cmd := "stat"
    args := #["-f", "%m %z", path.toString]  -- macOS stat format
  }
  match result.stdout.trim.splitOn " " with
  | [mtimeStr, sizeStr] =>
    return some {
      path := path
      modTime := mtimeStr.toNat?.getD 0
      size := sizeStr.toNat?.getD 0
    }
  | _ => return none

/-- Scan directory for file states -/
def scanDirectory (dir : System.FilePath) : IO (List FileState) := do
  if !(← dir.pathExists) then
    return []

  let mut states : List FileState := []
  let entries ← dir.readDir
  for entry in entries do
    if let some state ← getFileState entry.path then
      states := state :: states
  return states

/-- Detect changes between two states -/
def detectChanges (oldStates newStates : List FileState) : List String := Id.run do
  let mut changes : List String := []

  -- Find modified or new files
  for newState in newStates do
    match oldStates.find? (·.path == newState.path) with
    | some oldState =>
      if oldState.modTime != newState.modTime || oldState.size != newState.size then
        changes := s!"modified: {newState.path}" :: changes
    | none =>
      changes := s!"added: {newState.path}" :: changes

  -- Find deleted files
  for oldState in oldStates do
    if !newStates.any (·.path == oldState.path) then
      changes := s!"deleted: {oldState.path}" :: changes

  return changes.reverse

/-- Current unix timestamp -/
def watcherTimestamp : IO Nat := IO.monoMsNow

/-- Sleep for milliseconds -/
def sleepMs (ms : Nat) : IO Unit := do
  let _ ← IO.Process.run {
    cmd := "sleep"
    args := #[s!"{ms / 1000}.{(ms % 1000).repr.takeRight 3}"]
  }

/-- Run one poll cycle -/
def pollOnce (state : WatcherState) : IO (WatcherState × List String) := do
  let now ← watcherTimestamp
  let newStates ← scanDirectory state.config.beadsDir
  let changes := detectChanges state.fileStates newStates

  let newState := {
    state with
    lastScan := now
    fileStates := newStates
    changeCount := state.changeCount + changes.length
  }

  return (newState, changes)

/-- Format uptime -/
def formatUptime (seconds : Nat) : String :=
  if seconds < 60 then s!"{seconds}s"
  else if seconds < 3600 then s!"{seconds / 60}m {seconds % 60}s"
  else s!"{seconds / 3600}h {(seconds % 3600) / 60}m"

/-- Watcher status for JSON output -/
structure WatcherStatus where
  running : Bool
  uptime : String
  lastScan : Nat
  changeCount : Nat
  syncCount : Nat
  watchedFiles : Nat
  deriving Repr, Inhabited

instance : ToJson WatcherStatus where
  toJson s := Json.mkObj [
    ("running", Json.bool s.running),
    ("uptime", Json.str s.uptime),
    ("last_scan", Json.num s.lastScan),
    ("change_count", Json.num s.changeCount),
    ("sync_count", Json.num s.syncCount),
    ("watched_files", Json.num s.watchedFiles)
  ]

/-- Main watcher loop (runs until interrupted) -/
partial def runWatcher (config : WatcherConfig) (maxIterations : Option Nat := none) : IO Unit := do
  let startTime ← watcherTimestamp
  let initialStates ← scanDirectory config.beadsDir

  if config.verbose then
    IO.println s!"Starting watcher on {config.beadsDir}"
    IO.println s!"Watching {initialStates.length} files"
    IO.println s!"Poll interval: {config.pollIntervalMs}ms"
    if config.autoSync then
      IO.println "Auto-sync: enabled"

  let rec loop (state : WatcherState) (remaining : Option Nat) : IO Unit := do
    -- Check if we should stop
    match remaining with
    | some 0 => pure ()
    | _ =>
      sleepMs config.pollIntervalMs

      let (newState, changes) ← pollOnce state

      if !changes.isEmpty then
        if config.verbose then
          IO.println s!"\n[{← watcherTimestamp}] Detected {changes.length} change(s):"
          for change in changes do
            IO.println s!"  {change}"

        -- Auto-sync if enabled
        if config.autoSync then
          if config.verbose then
            IO.println "Triggering auto-sync..."
          -- Would call sync here
          pure ()

      loop newState (remaining.map (· - 1))

  let initialState : WatcherState := {
    config := config
    lastScan := startTime
    fileStates := initialStates
  }

  loop initialState maxIterations

/-- Get current watcher status (for status command) -/
def getWatcherStatus (_config : WatcherConfig) (startTime : Nat) (state : WatcherState) : IO WatcherStatus := do
  let now ← watcherTimestamp
  return {
    running := true
    uptime := formatUptime (now - startTime)
    lastScan := state.lastScan
    changeCount := state.changeCount
    syncCount := state.syncCount
    watchedFiles := state.fileStates.length
  }

end Beads.Sync.Watcher
