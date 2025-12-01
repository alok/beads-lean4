/-
Beads CLI - Command line interface for the Beads issue tracker
Agent-friendly with --json output on all commands
-/
import Beads
import Beads.Storage.Sqlite.Storage
import Lean.Data.Json

open Beads
open Lean Json

/-- Storage backend type -/
inductive StorageBackend where
  | jsonl
  | sqlite
  deriving Inhabited, BEq

/-- CLI configuration -/
structure CLIConfig where
  jsonOutput : Bool := false
  beadsDir : System.FilePath := ".beads"
  backend : StorageBackend := .jsonl

/-- Parse command line flags and return remaining args -/
def parseFlags (args : List String) : CLIConfig × List String :=
  let rec go (cfg : CLIConfig) (remaining : List String) (args : List String) : CLIConfig × List String :=
    match args with
    | [] => (cfg, remaining.reverse)
    | "--json" :: rest => go { cfg with jsonOutput := true } remaining rest
    | "-j" :: rest => go { cfg with jsonOutput := true } remaining rest
    | "--dir" :: dir :: rest => go { cfg with beadsDir := dir } remaining rest
    | "-d" :: dir :: rest => go { cfg with beadsDir := dir } remaining rest
    | "--sqlite" :: rest => go { cfg with backend := .sqlite } remaining rest
    | "--jsonl" :: rest => go { cfg with backend := .jsonl } remaining rest
    | arg :: rest => go cfg (arg :: remaining) rest
  go {} [] args

/-- Open storage based on backend selection -/
def openStorage (cfg : CLIConfig) : IO StorageOps := do
  match cfg.backend with
  | .jsonl =>
    let storage ← JsonlStorage.openOrCreate cfg.beadsDir
    pure storage.toStorageOps
  | .sqlite =>
    let dbPath := cfg.beadsDir / "beads.db"
    match ← SqliteStorage.openOrCreate dbPath with
    | .ok storage => pure storage.toStorageOps
    | .error e =>
      IO.eprintln s!"Failed to open SQLite database: {e}"
      -- Fallback to JSONL
      let storage ← JsonlStorage.openOrCreate cfg.beadsDir
      pure storage.toStorageOps

/-- Output JSON or text based on config -/
def outputResult (cfg : CLIConfig) (json : Json) (text : String) : IO Unit :=
  if cfg.jsonOutput then
    IO.println json.compress
  else
    IO.println text

/-- Format issue for text display -/
def formatIssueShort (issue : Issue) : String :=
  let statusStr := issue.status.toString
  let priorityStr := s!"P{issue.priority.val}"
  s!"{issue.id.value}  [{statusStr}] [{priorityStr}] {issue.title}"

/-- Format issue for detailed display -/
def formatIssueLong (issue : Issue) : String :=
  let lines := [
    s!"ID:          {issue.id.value}",
    s!"Title:       {issue.title}",
    s!"Status:      {issue.status.toString}",
    s!"Priority:    P{issue.priority.val}",
    s!"Type:        {issue.issueType.toString}",
    if issue.description.isEmpty then "" else s!"Description: {issue.description}",
    match issue.assignee with | some a => s!"Assignee:    {a}" | none => "",
    if issue.labels.isEmpty then "" else s!"Labels:      {", ".intercalate issue.labels}",
    match issue.estimatedMinutes with | some m => s!"Estimate:    {m} min" | none => ""
  ]
  "\n".intercalate (lines.filter (!·.isEmpty))

/-- Get current unix timestamp from system -/
def currentTimestamp : IO Nat := do
  let result ← IO.Process.output { cmd := "date", args := #["+%s"] }
  match result.stdout.trim.toNat? with
  | some n => pure n
  | none => pure 0  -- Fallback if date command fails

/-- Command: create a new issue -/
def cmdCreate (cfg : CLIConfig) (args : List String) : IO UInt32 := do
  match args with
  | title :: rest =>
    let ops ← openStorage cfg
    let now ← currentTimestamp

    -- Parse optional description from remaining args
    let description := if rest.isEmpty then "" else " ".intercalate rest

    let issue : Issue := {
      id := ⟨""⟩  -- Will be generated
      title := title
      description := description
      design := ""
      acceptanceCriteria := ""
      notes := ""
      status := .open
      priority := Priority.default
      issueType := .task
      assignee := none
      estimatedMinutes := none
      createdAt := now
      updatedAt := now
      closedAt := none
      closeReason := ""
      externalRef := none
      labels := []
    }

    let newId ← ops.createIssue issue "cli"
    ops.save

    let createdIssue := { issue with id := newId }
    let json := toJson createdIssue
    let text := s!"Created issue: {newId.value}"
    outputResult cfg json text
    pure 0
  | [] =>
    IO.eprintln "Error: create requires a title"
    IO.eprintln "Usage: beads create <title> [description...]"
    pure 1

/-- Command: list issues -/
def cmdList (cfg : CLIConfig) (args : List String) : IO UInt32 := do
  let ops ← openStorage cfg

  -- Parse filter options
  let rec parseFilter (filter : IssueFilter) (args : List String) : IssueFilter :=
    match args with
    | [] => filter
    | "--status" :: s :: rest =>
      let status := Status.fromString? s
      parseFilter { filter with status } rest
    | "--priority" :: p :: rest =>
      let priority := match p.toNat? with
        | some n => Priority.fromNat? n
        | none => none
      parseFilter { filter with priority } rest
    | "--type" :: t :: rest =>
      let issueType := IssueType.fromString? t
      parseFilter { filter with issueType } rest
    | "--assignee" :: a :: rest =>
      parseFilter { filter with assignee := some a } rest
    | "--search" :: s :: rest =>
      parseFilter { filter with titleSearch := s } rest
    | "--limit" :: n :: rest =>
      parseFilter { filter with limit := n.toNat?.getD 100 } rest
    | _ :: rest => parseFilter filter rest

  let filter := parseFilter IssueFilter.default args
  let issues ← ops.searchIssues filter

  if cfg.jsonOutput then
    let json := Json.arr (issues.map toJson).toArray
    IO.println json.compress
  else
    if issues.isEmpty then
      IO.println "No issues found"
    else
      for issue in issues do
        IO.println (formatIssueShort issue)
  pure 0

/-- Command: show issue details -/
def cmdShow (cfg : CLIConfig) (args : List String) : IO UInt32 := do
  match args.head? with
  | some idStr =>
    let ops ← openStorage cfg

    let issueId : IssueId := ⟨idStr⟩
    match ← ops.getIssue issueId with
    | some issue =>
      let deps ← ops.getDependencies issueId
      let dependents ← ops.getDependents issueId

      if cfg.jsonOutput then
        -- Include dependencies in JSON output
        let issueJson := toJson issue
        let finalJson : Json := match issueJson with
          | .obj fields =>
            let depsJson := Json.arr (deps.map toJson).toArray
            let dependentIds := dependents.map (·.issueId)
            let dependentsJson := Json.arr (dependentIds.map toJson).toArray
            .obj (fields.insert "dependencies" depsJson
                        |>.insert "dependents" dependentsJson)
          | _ => issueJson
        IO.println finalJson.compress
      else
        IO.println (formatIssueLong issue)
        if !deps.isEmpty then
          IO.println s!"\nBlocked by:"
          for dep in deps do
            IO.println s!"  - {dep.dependsOnId.value} ({dep.depType.toString})"
        if !dependents.isEmpty then
          IO.println s!"\nBlocking:"
          for dep in dependents do
            IO.println s!"  - {dep.issueId.value}"
      pure 0
    | none =>
      IO.eprintln s!"Error: Issue not found: {idStr}"
      pure 1
  | none =>
    IO.eprintln "Error: show requires an issue ID"
    IO.eprintln "Usage: beads show <issue-id>"
    pure 1

/-- Command: show ready work (unblocked issues) -/
def cmdReady (cfg : CLIConfig) (args : List String) : IO UInt32 := do
  let ops ← openStorage cfg

  let rec parseFilter (filter : WorkFilter) (args : List String) : WorkFilter :=
    match args with
    | [] => filter
    | "--assignee" :: a :: rest =>
      parseFilter { filter with assignee := some a } rest
    | "--unassigned" :: rest =>
      parseFilter { filter with unassigned := true } rest
    | "--limit" :: n :: rest =>
      parseFilter { filter with limit := n.toNat?.getD 20 } rest
    | _ :: rest => parseFilter filter rest

  let filter := parseFilter WorkFilter.default args
  let ready ← ops.getReadyWork filter

  if cfg.jsonOutput then
    let json := Json.arr (ready.map toJson).toArray
    IO.println json.compress
  else
    if ready.isEmpty then
      IO.println "No ready work found"
    else
      IO.println "Ready work (unblocked issues):"
      for issue in ready do
        IO.println (formatIssueShort issue)
  pure 0

/-- Command: add a dependency between issues -/
def cmdDepAdd (cfg : CLIConfig) (args : List String) : IO UInt32 := do
  match args with
  | fromId :: toId :: rest =>
    let ops ← openStorage cfg
    let now ← currentTimestamp

    -- Parse optional dependency type
    let depType := match rest with
      | "--type" :: t :: _ => DependencyType.fromString? t |>.getD .blocks
      | _ => .blocks

    let dep : Dependency := {
      issueId := ⟨fromId⟩
      dependsOnId := ⟨toId⟩
      depType := depType
      createdAt := now
      createdBy := "cli"
    }

    match ← ops.addDependency dep "cli" with
    | .ok () =>
      ops.save
      let json := toJson dep
      let text := s!"{fromId} now {depType.toString} {toId}"
      outputResult cfg json text
      pure 0
    | .error (.wouldCreateCycle _ _ _) =>
      IO.eprintln s!"Error: Adding this dependency would create a cycle"
      pure 1
  | _ =>
    IO.eprintln "Error: dep add requires two issue IDs"
    IO.eprintln "Usage: beads dep add <issue-id> <depends-on-id> [--type blocks|related|parent-child|discovered-from]"
    pure 1

/-- Command: update an issue -/
def cmdUpdate (cfg : CLIConfig) (args : List String) : IO UInt32 := do
  match args.head? with
  | some idStr =>
    let ops ← openStorage cfg
    let now ← currentTimestamp

    let issueId : IssueId := ⟨idStr⟩
    match ← ops.getIssue issueId with
    | some _ =>
      -- Parse update options from remaining args
      let rec parseUpdates (args : List String) (f : Issue → Issue) : Issue → Issue :=
        match args with
        | [] => f
        | "--title" :: t :: rest => parseUpdates rest (fun i => f { i with title := t })
        | "--description" :: d :: rest => parseUpdates rest (fun i => f { i with description := d })
        | "--status" :: s :: rest =>
          let status := Status.fromString? s
          parseUpdates rest (fun i => f { i with status := status.getD i.status })
        | "--priority" :: p :: rest =>
          let priority := p.toNat?.bind Priority.fromNat?
          parseUpdates rest (fun i => f { i with priority := priority.getD i.priority })
        | "--assignee" :: a :: rest =>
          let assignee := if a == "none" || a == "" then none else some a
          parseUpdates rest (fun i => f { i with assignee := assignee })
        | "--type" :: t :: rest =>
          let issueType := IssueType.fromString? t
          parseUpdates rest (fun i => f { i with issueType := issueType.getD i.issueType })
        | _ :: rest => parseUpdates rest f

      let updates := parseUpdates args.tail! (fun i => { i with updatedAt := now })
      ops.updateIssue issueId updates "cli"
      ops.save

      match ← ops.getIssue issueId with
      | some updated =>
        let json := toJson updated
        let text := s!"Updated issue: {idStr}"
        outputResult cfg json text
        pure 0
      | none =>
        IO.eprintln "Error: Issue disappeared after update"
        pure 1
    | none =>
      IO.eprintln s!"Error: Issue not found: {idStr}"
      pure 1
  | none =>
    IO.eprintln "Error: update requires an issue ID"
    IO.eprintln "Usage: beads update <id> [--title <t>] [--status <s>] [--priority <p>] [--assignee <a>] ..."
    pure 1

/-- Command: close an issue -/
def cmdClose (cfg : CLIConfig) (args : List String) : IO UInt32 := do
  match args.head? with
  | some idStr =>
    let ops ← openStorage cfg
    let now ← currentTimestamp

    let issueId : IssueId := ⟨idStr⟩
    match ← ops.getIssue issueId with
    | some issue =>
      if issue.status == .closed then
        IO.eprintln s!"Issue {idStr} is already closed"
        pure 1
      else
        -- Parse optional reason
        let reason := match args.tail! with
          | "--reason" :: r :: _ => r
          | r :: _ => if r.startsWith "--" then "" else r
          | _ => ""

        ops.updateIssue issueId (fun i => { i with
          status := .closed
          closedAt := some now
          closeReason := reason
          updatedAt := now
        }) "cli"
        ops.save

        match ← ops.getIssue issueId with
        | some updated =>
          let json := toJson updated
          let text := s!"Closed issue: {idStr}"
          outputResult cfg json text
          pure 0
        | none =>
          IO.eprintln "Error: Issue disappeared after close"
          pure 1
    | none =>
      IO.eprintln s!"Error: Issue not found: {idStr}"
      pure 1
  | none =>
    IO.eprintln "Error: close requires an issue ID"
    IO.eprintln "Usage: beads close <id> [reason | --reason <text>]"
    pure 1

/-- Command: add a label to an issue -/
def cmdLabelAdd (cfg : CLIConfig) (args : List String) : IO UInt32 := do
  match args with
  | issueIdStr :: label :: _ =>
    let ops ← openStorage cfg

    let issueId : IssueId := ⟨issueIdStr⟩
    match ← ops.getIssue issueId with
    | some _ =>
      ops.addLabel issueId label "cli"
      ops.save
      let labels ← ops.getLabels issueId
      let json := Json.mkObj [("id", toJson issueId), ("labels", Json.arr (labels.map Json.str).toArray)]
      let text := s!"Added label '{label}' to {issueIdStr}"
      outputResult cfg json text
      pure 0
    | none =>
      IO.eprintln s!"Error: Issue not found: {issueIdStr}"
      pure 1
  | _ =>
    IO.eprintln "Error: label add requires issue ID and label"
    IO.eprintln "Usage: beads label add <issue-id> <label>"
    pure 1

/-- Command: remove a label from an issue -/
def cmdLabelRemove (cfg : CLIConfig) (args : List String) : IO UInt32 := do
  match args with
  | issueIdStr :: label :: _ =>
    let ops ← openStorage cfg

    let issueId : IssueId := ⟨issueIdStr⟩
    match ← ops.getIssue issueId with
    | some _ =>
      ops.removeLabel issueId label "cli"
      ops.save
      let labels ← ops.getLabels issueId
      let json := Json.mkObj [("id", toJson issueId), ("labels", Json.arr (labels.map Json.str).toArray)]
      let text := s!"Removed label '{label}' from {issueIdStr}"
      outputResult cfg json text
      pure 0
    | none =>
      IO.eprintln s!"Error: Issue not found: {issueIdStr}"
      pure 1
  | _ =>
    IO.eprintln "Error: label remove requires issue ID and label"
    IO.eprintln "Usage: beads label remove <issue-id> <label>"
    pure 1

/-- Command: list labels on an issue -/
def cmdLabelList (cfg : CLIConfig) (args : List String) : IO UInt32 := do
  match args.head? with
  | some issueIdStr =>
    let ops ← openStorage cfg

    let issueId : IssueId := ⟨issueIdStr⟩
    match ← ops.getIssue issueId with
    | some _ =>
      let labels ← ops.getLabels issueId
      if cfg.jsonOutput then
        let json := Json.arr (labels.map Json.str).toArray
        IO.println json.compress
      else
        if labels.isEmpty then
          IO.println s!"No labels on {issueIdStr}"
        else
          IO.println s!"Labels on {issueIdStr}: {", ".intercalate labels}"
      pure 0
    | none =>
      IO.eprintln s!"Error: Issue not found: {issueIdStr}"
      pure 1
  | none =>
    IO.eprintln "Error: label list requires an issue ID"
    IO.eprintln "Usage: beads label list <issue-id>"
    pure 1

/-- Command: show statistics -/
def cmdStats (cfg : CLIConfig) (_args : List String) : IO UInt32 := do
  let ops ← openStorage cfg

  let allIssues ← ops.getAllIssues
  let blocked ← ops.getBlockedIssues
  let ready ← ops.getReadyWork WorkFilter.default

  let total := allIssues.length
  let openCount := allIssues.filter (·.status == .open) |>.length
  let inProgressCount := allIssues.filter (·.status == .inProgress) |>.length
  let closedCount := allIssues.filter (·.status == .closed) |>.length
  let blockedCount := blocked.length
  let readyCount := ready.length

  let stats : Statistics := {
    totalIssues := total
    openIssues := openCount
    inProgressIssues := inProgressCount
    closedIssues := closedCount
    blockedIssues := blockedCount
    readyIssues := readyCount
    epicsEligibleForClosure := 0  -- TODO: implement
  }

  if cfg.jsonOutput then
    IO.println (toJson stats).compress
  else
    IO.println s!"Total:       {total}"
    IO.println s!"Open:        {openCount}"
    IO.println s!"In Progress: {inProgressCount}"
    IO.println s!"Closed:      {closedCount}"
    IO.println s!"Blocked:     {blockedCount}"
    IO.println s!"Ready:       {readyCount}"
  pure 0

/-- Helper for dependency tree collection -/
def collectTreeLoop (ops : StorageOps) (allDeps : List Dependency) (rootId : IssueId)
    (queue : List (IssueId × Nat)) (visited : Std.HashSet IssueId)
    (acc : List TreeNode) (fuel : Nat) : IO (List TreeNode) := do
  if fuel == 0 then pure acc.reverse
  else match queue with
  | [] => pure acc.reverse
  | (id, depth) :: rest =>
    if visited.contains id then
      collectTreeLoop ops allDeps rootId rest visited acc (fuel - 1)
    else
      match ← ops.getIssue id with
      | some issue =>
        let node : TreeNode := {
          issue := issue
          depth := depth
          parentId := if depth == 0 then none else some rootId
          truncated := depth >= 5
        }
        if depth >= 5 then
          collectTreeLoop ops allDeps rootId rest (visited.insert id) (node :: acc) (fuel - 1)
        else
          -- Get issues this one depends on (blocking dependencies)
          let blocking := allDeps.filter fun d =>
            d.issueId == id && d.depType.affectsBlocking
          let children := blocking.map fun d => (d.dependsOnId, depth + 1)
          collectTreeLoop ops allDeps rootId (rest ++ children) (visited.insert id) (node :: acc) (fuel - 1)
      | none =>
        collectTreeLoop ops allDeps rootId rest (visited.insert id) acc (fuel - 1)
termination_by fuel
decreasing_by all_goals simp_all; omega

/-- Command: show dependency tree -/
def cmdDepTree (cfg : CLIConfig) (args : List String) : IO UInt32 := do
  match args.head? with
  | some issueIdStr =>
    let ops ← openStorage cfg

    let issueId : IssueId := ⟨issueIdStr⟩
    match ← ops.getIssue issueId with
    | some _ =>
      let allDeps ← ops.getAllDependencies
      let tree ← collectTreeLoop ops allDeps issueId [(issueId, 0)] {} [] 100

      if cfg.jsonOutput then
        let json := Json.arr (tree.map fun node =>
          Json.mkObj [
            ("id", toJson node.issue.id),
            ("title", Json.str node.issue.title),
            ("status", toJson node.issue.status),
            ("depth", Json.num node.depth),
            ("truncated", Json.bool node.truncated)
          ]
        ).toArray
        IO.println json.compress
      else
        for node in tree do
          let indent := String.ofList (List.replicate (node.depth * 2) ' ')
          let statusMark := if node.issue.status == .closed then "✓" else "○"
          let truncMark := if node.truncated then " ..." else ""
          IO.println s!"{indent}{statusMark} {node.issue.id.value}: {node.issue.title}{truncMark}"
      pure 0
    | none =>
      IO.eprintln s!"Error: Issue not found: {issueIdStr}"
      pure 1
  | none =>
    IO.eprintln "Error: dep tree requires an issue ID"
    IO.eprintln "Usage: beads dep tree <issue-id>"
    pure 1

/-- Command: show blocked issues -/
def cmdBlocked (cfg : CLIConfig) (_args : List String) : IO UInt32 := do
  let ops ← openStorage cfg

  let blocked ← ops.getBlockedIssues

  if cfg.jsonOutput then
    let json := Json.arr (blocked.map toJson).toArray
    IO.println json.compress
  else
    if blocked.isEmpty then
      IO.println "No blocked issues"
    else
      IO.println "Blocked issues:"
      for bi in blocked do
        let blockerIds := ", ".intercalate (bi.blockedBy.map (·.value))
        IO.println s!"{formatIssueShort bi.issue}"
        IO.println s!"  blocked by: {blockerIds}"
  pure 0

/-- Print help message -/
def printHelp : IO Unit := do
  IO.println "beads - git-backed distributed issue tracker (Lean 4 port)"
  IO.println ""
  IO.println "Usage: beads [--json] <command> [args...]"
  IO.println ""
  IO.println "Commands:"
  IO.println "  create <title> [description]   Create a new issue"
  IO.println "  list [filters]                 List issues"
  IO.println "  show <id>                      Show issue details"
  IO.println "  update <id> [options]          Update an issue"
  IO.println "  close <id> [reason]            Close an issue"
  IO.println "  label add <id> <label>         Add label to issue"
  IO.println "  label remove <id> <label>      Remove label from issue"
  IO.println "  label list <id>                List labels on issue"
  IO.println "  dep add <from> <to> [--type]   Add dependency (from blocks to)"
  IO.println "  dep tree <id>                  Show dependency tree"
  IO.println "  ready [filters]                Show ready (unblocked) work"
  IO.println "  blocked                        Show blocked issues"
  IO.println "  stats                          Show issue statistics"
  IO.println "  help                           Show this help"
  IO.println ""
  IO.println "Flags:"
  IO.println "  --json, -j                     Output in JSON format"
  IO.println "  --dir, -d <path>               Use custom .beads directory"
  IO.println "  --sqlite                       Use SQLite backend (default: JSONL)"
  IO.println "  --jsonl                        Use JSONL backend (default)"
  IO.println ""
  IO.println "Update options:"
  IO.println "  --title <text>                 Set title"
  IO.println "  --description <text>           Set description"
  IO.println "  --status <status>              Set status"
  IO.println "  --priority <0-4>               Set priority"
  IO.println "  --assignee <name|none>         Set assignee"
  IO.println "  --type <type>                  Set issue type"
  IO.println ""
  IO.println "List filters:"
  IO.println "  --status <status>              Filter by status (open, in_progress, closed)"
  IO.println "  --priority <0-4>               Filter by priority"
  IO.println "  --type <type>                  Filter by type (task, bug, feature, epic)"
  IO.println "  --assignee <name>              Filter by assignee"
  IO.println "  --search <text>                Search in title"
  IO.println "  --limit <n>                    Limit results"
  IO.println ""
  IO.println "Dependency types: blocks, related, parent-child, discovered-from"

def main (args : List String) : IO UInt32 := do
  let (cfg, cmdArgs) := parseFlags args

  match cmdArgs with
  | [] =>
    printHelp
    pure 0
  | "help" :: _ =>
    printHelp
    pure 0
  | "create" :: rest => cmdCreate cfg rest
  | "list" :: rest => cmdList cfg rest
  | "show" :: rest => cmdShow cfg rest
  | "update" :: rest => cmdUpdate cfg rest
  | "close" :: rest => cmdClose cfg rest
  | "label" :: "add" :: rest => cmdLabelAdd cfg rest
  | "label" :: "remove" :: rest => cmdLabelRemove cfg rest
  | "label" :: "list" :: rest => cmdLabelList cfg rest
  | "label" :: rest => cmdLabelList cfg rest  -- Default to list
  | "dep" :: "add" :: rest => cmdDepAdd cfg rest
  | "dep" :: "tree" :: rest => cmdDepTree cfg rest
  | "ready" :: rest => cmdReady cfg rest
  | "blocked" :: rest => cmdBlocked cfg rest
  | "stats" :: rest => cmdStats cfg rest
  | cmd :: _ =>
    IO.eprintln s!"Unknown command: {cmd}"
    IO.eprintln "Run 'beads help' for usage"
    pure 1
