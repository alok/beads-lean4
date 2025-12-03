/-
Beads Git Module

Wrapper for git operations used during sync.
Provides detection of git state, commit, pull, push operations.

Port of: /Users/alokbeniwal/beads/cmd/bd/sync.go git functions
-/
import Beads.Core.Types

namespace Beads.Sync

/-- Result of a git command -/
structure GitResult where
  success : Bool
  output : String
  exitCode : UInt32
  deriving Repr, Inhabited

/-- Run a git command and capture output -/
def runGit (args : List String) : IO GitResult := do
  let proc ← IO.Process.spawn {
    cmd := "git"
    args := args.toArray
    stdout := .piped
    stderr := .piped
  }
  let stdout ← proc.stdout.readToEnd
  let stderr ← proc.stderr.readToEnd
  let exitCode ← proc.wait
  return {
    success := exitCode == 0
    output := if stdout.isEmpty then stderr else stdout
    exitCode := exitCode
  }

/-- Check if current directory is in a git repository -/
def isGitRepo : IO Bool := do
  let result ← runGit ["rev-parse", "--git-dir"]
  return result.success

/-- Get the current branch name -/
def currentBranch : IO (Option String) := do
  let result ← runGit ["symbolic-ref", "--short", "HEAD"]
  if result.success then
    return some result.output.trim
  else
    return none

/-- Check if the current branch has an upstream configured -/
def hasUpstream : IO Bool := do
  match ← currentBranch with
  | none => return false
  | some branch =>
    let remoteResult ← runGit ["config", "--get", s!"branch.{branch}.remote"]
    let mergeResult ← runGit ["config", "--get", s!"branch.{branch}.merge"]
    return remoteResult.success && mergeResult.success

/-- Check if any git remote exists -/
def hasRemote : IO Bool := do
  let result ← runGit ["remote"]
  return result.success && !result.output.trim.isEmpty

/-- Check for uncommitted changes in the .beads directory -/
def hasBeadsChanges (beadsDir : System.FilePath) : IO Bool := do
  let result ← runGit ["status", "--porcelain", beadsDir.toString]
  return result.success && !result.output.trim.isEmpty

/-- Check for unmerged paths (merge in progress) -/
def hasUnmergedPaths : IO Bool := do
  let result ← runGit ["status", "--porcelain"]
  if !result.success then return false

  -- Check for unmerged status codes (DD, AU, UD, UA, DU, AA, UU)
  for line in result.output.splitOn "\n" do
    if line.length >= 2 then
      let s := line.take 2
      if s == "DD" || s == "AU" || s == "UD" || s == "UA" || s == "DU" || s == "AA" || s == "UU" then
        return true

  -- Also check if MERGE_HEAD exists
  let mergeHead ← runGit ["rev-parse", "-q", "--verify", "MERGE_HEAD"]
  return mergeHead.success

/-- Stage files in the .beads directory -/
def stageBeadsDir (beadsDir : System.FilePath) : IO GitResult := do
  runGit ["add", beadsDir.toString]

/-- Commit staged changes -/
def commit (message : String) : IO GitResult := do
  runGit ["commit", "-m", message]

/-- Commit only .beads directory changes -/
def commitBeadsDir (beadsDir : System.FilePath) (message : String) : IO GitResult := do
  -- Stage specific sync files
  let syncFiles := [
    (beadsDir / "issues.jsonl").toString,
    (beadsDir / "deletions.jsonl").toString,
    (beadsDir / "metadata.json").toString
  ]

  -- Add existing files
  for file in syncFiles do
    if ← System.FilePath.pathExists file then
      let _ ← runGit ["add", file]

  -- Commit only .beads/ files
  runGit ["commit", "-m", message, "--", beadsDir.toString]

/-- Pull from upstream -/
def pull : IO GitResult := do
  -- Check if remote exists
  if !(← hasRemote) then
    return { success := true, output := "No remote configured (local-only mode)", exitCode := 0 }

  -- Get current branch
  match ← currentBranch with
  | none => return { success := false, output := "Failed to get current branch", exitCode := 1 }
  | some branch =>
    -- Get remote name
    let remoteResult ← runGit ["config", "--get", s!"branch.{branch}.remote"]
    let remote := if remoteResult.success then remoteResult.output.trim else "origin"
    runGit ["pull", remote, branch]

/-- Push to upstream -/
def push : IO GitResult := do
  -- Check if remote exists
  if !(← hasRemote) then
    return { success := true, output := "No remote configured (local-only mode)", exitCode := 0 }

  runGit ["push"]

/-- Get the default branch name (main or master) -/
def getDefaultBranch : IO String := do
  -- Try to get from remote HEAD
  let result ← runGit ["symbolic-ref", "refs/remotes/origin/HEAD"]
  if result.success then
    let ref := result.output.trim
    if ref.startsWith "refs/remotes/origin/" then
      return ref.drop 20  -- Strip "refs/remotes/origin/"

  -- Fallback: check if origin/main exists
  let mainResult ← runGit ["rev-parse", "--verify", "origin/main"]
  if mainResult.success then return "main"

  -- Fallback: check if origin/master exists
  let masterResult ← runGit ["rev-parse", "--verify", "origin/master"]
  if masterResult.success then return "master"

  -- Default to main
  return "main"

/-- Fetch from a specific remote and branch -/
def fetch (remote : String) (branch : String) : IO GitResult := do
  runGit ["fetch", remote, branch]

/-- Checkout a path from a specific ref -/
def checkoutPath (ref : String) (path : System.FilePath) : IO GitResult := do
  runGit ["checkout", ref, "--", path.toString]

/-- Get the git repository root directory -/
def getRepoRoot : IO (Option System.FilePath) := do
  let result ← runGit ["rev-parse", "--show-toplevel"]
  if result.success then
    return some result.output.trim
  else
    return none

/-- Information about current git state -/
structure GitStatus where
  isRepo : Bool
  currentBranch : Option String
  hasUpstream : Bool
  hasRemote : Bool
  hasUnmerged : Bool
  hasBeadsChanges : Bool
  deriving Repr, Inhabited

/-- Get comprehensive git status -/
def getGitStatus (beadsDir : System.FilePath) : IO GitStatus := do
  let isRepo ← isGitRepo
  if !isRepo then
    return { isRepo := false, currentBranch := none, hasUpstream := false,
             hasRemote := false, hasUnmerged := false, hasBeadsChanges := false }

  return {
    isRepo := true
    currentBranch := ← currentBranch
    hasUpstream := ← hasUpstream
    hasRemote := ← hasRemote
    hasUnmerged := ← hasUnmergedPaths
    hasBeadsChanges := ← hasBeadsChanges beadsDir
  }

end Beads.Sync
