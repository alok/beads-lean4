/-
Beads Activity Log

Records all issue operations to .beads/activity.jsonl for auditing and history.
Each entry records: timestamp, actor, action, issue_id, and details.
-/
import Lean.Data.Json
import Beads.Core

namespace Beads.Sync.ActivityLog

open Lean Json

/-- Activity entry action types -/
inductive Action where
  | create
  | update
  | close
  | reopen
  | delete
  | addDep
  | removeDep
  | addLabel
  | removeLabel
  | addComment
  | assign
  | unassign
  | sync
  deriving Repr, Inhabited, BEq

def Action.toString : Action → String
  | .create => "create"
  | .update => "update"
  | .close => "close"
  | .reopen => "reopen"
  | .delete => "delete"
  | .addDep => "add_dependency"
  | .removeDep => "remove_dependency"
  | .addLabel => "add_label"
  | .removeLabel => "remove_label"
  | .addComment => "add_comment"
  | .assign => "assign"
  | .unassign => "unassign"
  | .sync => "sync"

def Action.fromString? : String → Option Action
  | "create" => some .create
  | "update" => some .update
  | "close" => some .close
  | "reopen" => some .reopen
  | "delete" => some .delete
  | "add_dependency" => some .addDep
  | "remove_dependency" => some .removeDep
  | "add_label" => some .addLabel
  | "remove_label" => some .removeLabel
  | "add_comment" => some .addComment
  | "assign" => some .assign
  | "unassign" => some .unassign
  | "sync" => some .sync
  | _ => none

instance : ToJson Action where
  toJson a := Json.str a.toString

instance : FromJson Action where
  fromJson? j := do
    let s ← j.getStr?
    match Action.fromString? s with
    | some a => pure a
    | none => throw s!"Unknown action: {s}"

/-- Activity log entry -/
structure ActivityEntry where
  timestamp : Nat
  actor : String
  action : Action
  issueId : Option String  -- None for sync actions
  title : Option String    -- For better readability
  details : String         -- Human-readable details
  deriving Repr, Inhabited

instance : ToJson ActivityEntry where
  toJson e := Json.mkObj [
    ("timestamp", Json.num e.timestamp),
    ("actor", Json.str e.actor),
    ("action", toJson e.action),
    ("issue_id", match e.issueId with | some id => Json.str id | none => Json.null),
    ("title", match e.title with | some t => Json.str t | none => Json.null),
    ("details", Json.str e.details)
  ]

instance : FromJson ActivityEntry where
  fromJson? j := do
    let timestamp ← (j.getObjVal? "timestamp" >>= (·.getNat?))
    let actor ← j.getObjValAs? String "actor"
    let action ← j.getObjValAs? Action "action"
    let issueId := (j.getObjVal? "issue_id").toOption.bind (·.getStr?.toOption)
    let title := (j.getObjVal? "title").toOption.bind (·.getStr?.toOption)
    let details ← j.getObjValAs? String "details"
    pure { timestamp, actor, action, issueId, title, details }

/-- Activity log manager -/
structure ActivityLog where
  logPath : System.FilePath
  deriving Inhabited

/-- Create activity log for a beads directory -/
def ActivityLog.forDirectory (beadsDir : System.FilePath) : ActivityLog :=
  { logPath := beadsDir / "activity.jsonl" }

/-- Append an entry to the activity log -/
def ActivityLog.append (log : ActivityLog) (entry : ActivityEntry) : IO Unit := do
  let line := (toJson entry).compress ++ "\n"
  let h ← IO.FS.Handle.mk log.logPath .append
  h.putStr line

/-- Read all entries from the activity log -/
def ActivityLog.readAll (log : ActivityLog) : IO (List ActivityEntry) := do
  if !(← log.logPath.pathExists) then
    return []
  let content ← IO.FS.readFile log.logPath
  let lines := content.splitOn "\n" |>.filter (!·.isEmpty)
  let entries := lines.filterMap fun line =>
    match Json.parse line >>= fromJson? with
    | .ok entry => some entry
    | .error _ => none
  return entries

/-- Read the most recent N entries (newest first) -/
def ActivityLog.readRecent (log : ActivityLog) (n : Nat) : IO (List ActivityEntry) := do
  let all ← log.readAll
  -- Sort by timestamp descending and take first n
  let sorted := all.toArray.qsort (fun a b => a.timestamp > b.timestamp)
  return sorted.toList.take n

/-- Get entries for a specific issue -/
def ActivityLog.forIssue (log : ActivityLog) (issueId : String) : IO (List ActivityEntry) := do
  let all ← log.readAll
  return all.filter (·.issueId == some issueId)

/-- Get entries since a timestamp -/
def ActivityLog.since (log : ActivityLog) (timestamp : Nat) : IO (List ActivityEntry) := do
  let all ← log.readAll
  return all.filter (·.timestamp >= timestamp)

/-- Helper: format timestamp as human-readable string -/
def formatActivityTimestamp (ts : Nat) : IO String := do
  let result ← IO.Process.output {
    cmd := "date",
    args := #["-r", toString ts, "+%Y-%m-%d %H:%M:%S"]
  }
  pure result.stdout.trim

/-- Format an activity entry for display -/
def ActivityEntry.format (entry : ActivityEntry) : IO String := do
  let time ← formatActivityTimestamp entry.timestamp
  let issueStr := match entry.issueId with
    | some id => id
    | none => "system"
  pure s!"[{time}] {entry.actor}: {entry.action.toString} {issueStr} - {entry.details}"

end Beads.Sync.ActivityLog
