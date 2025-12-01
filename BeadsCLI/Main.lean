/-
Beads CLI - Command line interface for the Beads issue tracker
Agent-friendly with --json output on all commands
-/
import Beads
import Lean.Data.Json

open Beads
open Lean Json

/-- CLI configuration -/
structure CLIConfig where
  jsonOutput : Bool := false
  beadsDir : System.FilePath := ".beads"

/-- Parse command line flags and return remaining args -/
def parseFlags (args : List String) : CLIConfig × List String :=
  let rec go (cfg : CLIConfig) (remaining : List String) (args : List String) : CLIConfig × List String :=
    match args with
    | [] => (cfg, remaining.reverse)
    | "--json" :: rest => go { cfg with jsonOutput := true } remaining rest
    | "-j" :: rest => go { cfg with jsonOutput := true } remaining rest
    | "--dir" :: dir :: rest => go { cfg with beadsDir := dir } remaining rest
    | "-d" :: dir :: rest => go { cfg with beadsDir := dir } remaining rest
    | arg :: rest => go cfg (arg :: remaining) rest
  go {} [] args

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

/-- Get current unix timestamp (simplified) -/
def currentTimestamp : IO Nat := do
  -- Use system time if available, otherwise return a placeholder
  pure 1700000000  -- TODO: proper timestamp

/-- Command: create a new issue -/
def cmdCreate (cfg : CLIConfig) (args : List String) : IO UInt32 := do
  match args with
  | title :: rest =>
    let storage ← JsonlStorage.openOrCreate cfg.beadsDir
    let ops := storage.toStorageOps
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
  let storage ← JsonlStorage.openOrCreate cfg.beadsDir
  let ops := storage.toStorageOps

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
    let storage ← JsonlStorage.openOrCreate cfg.beadsDir
    let ops := storage.toStorageOps

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
  let storage ← JsonlStorage.openOrCreate cfg.beadsDir
  let ops := storage.toStorageOps

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
    let storage ← JsonlStorage.openOrCreate cfg.beadsDir
    let ops := storage.toStorageOps
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
    let storage ← JsonlStorage.openOrCreate cfg.beadsDir
    let ops := storage.toStorageOps
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
    let storage ← JsonlStorage.openOrCreate cfg.beadsDir
    let ops := storage.toStorageOps
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

/-- Command: show blocked issues -/
def cmdBlocked (cfg : CLIConfig) (_args : List String) : IO UInt32 := do
  let storage ← JsonlStorage.openOrCreate cfg.beadsDir
  let ops := storage.toStorageOps

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
  IO.println "  dep add <from> <to> [--type]   Add dependency (from blocks to)"
  IO.println "  ready [filters]                Show ready (unblocked) work"
  IO.println "  blocked                        Show blocked issues"
  IO.println "  help                           Show this help"
  IO.println ""
  IO.println "Flags:"
  IO.println "  --json, -j                     Output in JSON format"
  IO.println "  --dir, -d <path>               Use custom .beads directory"
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
  | "dep" :: "add" :: rest => cmdDepAdd cfg rest
  | "ready" :: rest => cmdReady cfg rest
  | "blocked" :: rest => cmdBlocked cfg rest
  | cmd :: _ =>
    IO.eprintln s!"Unknown command: {cmd}"
    IO.eprintln "Run 'beads help' for usage"
    pure 1
