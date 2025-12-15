/-
SQLite Storage Implementation
Implements StorageOps interface using SQLite database
-/
import Beads.Storage.Storage
import Beads.Storage.Sqlite.FFI
import Beads.Core.Id

namespace Beads

open Sqlite

/-- SQLite Storage state -/
structure SqliteStorage where
  /-- Database connection -/
  db : Database
  /-- Path to database file -/
  dbPath : System.FilePath
  /-- ID generator state -/
  idGenState : IO.Ref IdGenerator.State

namespace SqliteStorage

/-- SQL to create schema -/
private def createSchemaSQL : List String := [
  "CREATE TABLE IF NOT EXISTS issues (
    id TEXT PRIMARY KEY,
    title TEXT NOT NULL,
    description TEXT DEFAULT '',
    design TEXT DEFAULT '',
    acceptance_criteria TEXT DEFAULT '',
    notes TEXT DEFAULT '',
    status TEXT NOT NULL DEFAULT 'open',
    priority INTEGER NOT NULL DEFAULT 2,
    issue_type TEXT NOT NULL DEFAULT 'task',
    assignee TEXT,
    estimated_minutes INTEGER,
    created_at INTEGER NOT NULL,
    updated_at INTEGER NOT NULL,
    closed_at INTEGER,
    close_reason TEXT DEFAULT '',
    external_ref TEXT
  )",
  "CREATE TABLE IF NOT EXISTS dependencies (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    issue_id TEXT NOT NULL REFERENCES issues(id) ON DELETE CASCADE,
    depends_on_id TEXT NOT NULL REFERENCES issues(id) ON DELETE CASCADE,
    dep_type TEXT NOT NULL DEFAULT 'blocks',
    created_at INTEGER NOT NULL,
    created_by TEXT NOT NULL,
    UNIQUE(issue_id, depends_on_id)
  )",
  "CREATE TABLE IF NOT EXISTS labels (
    issue_id TEXT NOT NULL REFERENCES issues(id) ON DELETE CASCADE,
    label TEXT NOT NULL,
    PRIMARY KEY (issue_id, label)
  )",
  "CREATE TABLE IF NOT EXISTS comments (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    issue_id TEXT NOT NULL REFERENCES issues(id) ON DELETE CASCADE,
    author TEXT NOT NULL,
    text TEXT NOT NULL,
    created_at INTEGER NOT NULL
  )",
  "CREATE TABLE IF NOT EXISTS messages (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    from_agent TEXT NOT NULL,
    to_agent TEXT NOT NULL,
    subject TEXT NOT NULL,
    body TEXT NOT NULL,
    created_at INTEGER NOT NULL,
    read_at INTEGER,
    acked_at INTEGER,
    related_issue TEXT REFERENCES issues(id) ON DELETE SET NULL
  )",
  "CREATE INDEX IF NOT EXISTS idx_comments_issue_id ON comments(issue_id)",
  "CREATE INDEX IF NOT EXISTS idx_messages_to ON messages(to_agent)",
  "CREATE INDEX IF NOT EXISTS idx_issues_status ON issues(status)",
  "CREATE INDEX IF NOT EXISTS idx_issues_priority ON issues(priority)",
  "CREATE INDEX IF NOT EXISTS idx_dependencies_issue_id ON dependencies(issue_id)",
  "CREATE INDEX IF NOT EXISTS idx_dependencies_depends_on_id ON dependencies(depends_on_id)"
]

/-- Create a new SQLite storage at the given path -/
def create (dbPath : System.FilePath) : IO (Except String SqliteStorage) := do
  match ← Database.openOrCreate dbPath.toString with
  | .error e => return .error s!"Failed to open database: {e}"
  | .ok db =>
    -- Create schema
    match ← Database.execMany db createSchemaSQL with
    | .error e => return .error s!"Failed to create schema: {e}"
    | .ok () =>
      -- Load existing IDs
      let existingIds ← loadExistingIds db
      let idGenState ← IO.mkRef { existingIds := existingIds }
      return .ok { db, dbPath, idGenState }
where
  loadExistingIds (db : Database) : IO (List String) := do
    match ← prepare db "SELECT id FROM issues" with
    | .error _ => return []
    | .ok stmt =>
      let mut ids : List String := []
      let mut running := true
      while running do
        let rc ← step stmt
        if rc == SQLITE_ROW then
          let id ← columnText stmt 0
          ids := id :: ids
        else
          running := false
      finalize stmt
      return ids

/-- Parse status from string -/
private def parseStatus (s : String) : Status :=
  match Status.fromString? s with
  | some st => st
  | none => .«open»

/-- Parse issue type from string -/
private def parseIssueType (s : String) : IssueType :=
  match IssueType.fromString? s with
  | some t => t
  | none => .task

/-- Parse priority from int -/
private def parsePriority (n : Int) : Priority :=
  let n' := n.toNat
  if h : n' < 5 then ⟨n', h⟩ else Priority.default

/-- Parse dependency type from string -/
private def parseDepType (s : String) : DependencyType :=
  match DependencyType.fromString? s with
  | some t => t
  | none => .blocks

/-- Read a dependency from a statement (assumes positioned on a row) -/
private def readDependency (stmt : Statement) : IO Dependency := do
  let issueId ← columnText stmt 0
  let dependsOnId ← columnText stmt 1
  let depType ← columnText stmt 2
  let createdAt ← columnInt stmt 3
  let createdBy ← columnText stmt 4
  return {
    issueId := ⟨issueId⟩
    dependsOnId := ⟨dependsOnId⟩
    depType := parseDepType depType
    createdAt := createdAt.toNat
    createdBy
  }

/-- Read an issue from a statement (assumes positioned on a row) -/
private def readIssue (stmt : Statement) : IO Issue := do
  let id ← columnText stmt 0
  let title ← columnText stmt 1
  let description ← columnText stmt 2
  let design ← columnText stmt 3
  let acceptanceCriteria ← columnText stmt 4
  let notes ← columnText stmt 5
  let status ← columnText stmt 6
  let priority ← columnInt stmt 7
  let issueType ← columnText stmt 8
  let assignee ← Statement.columnTextOpt stmt 9
  let estimatedMinutes ← Statement.columnIntOpt stmt 10
  let createdAt ← columnInt stmt 11
  let updatedAt ← columnInt stmt 12
  let closedAt ← Statement.columnIntOpt stmt 13
  let closeReason ← columnText stmt 14
  let externalRef ← Statement.columnTextOpt stmt 15

  return {
    id := ⟨id⟩
    title := title
    description := description
    design := design
    acceptanceCriteria := acceptanceCriteria
    notes := notes
    status := parseStatus status
    priority := parsePriority priority
    issueType := parseIssueType issueType
    assignee := assignee
    estimatedMinutes := estimatedMinutes.map Int.toNat
    createdAt := createdAt.toNat
    updatedAt := updatedAt.toNat
    closedAt := closedAt.map Int.toNat
    closeReason := closeReason
    externalRef := externalRef
    labels := []  -- Load separately if needed
  }

/-- Create storage operations from SqliteStorage -/
def toStorageOps (storage : SqliteStorage) : StorageOps := {
  createIssue := fun issue _actor => do
    let idState ← storage.idGenState.get
    let (newId, newIdState) := IdGenerator.generateId idState issue.title issue.description issue.createdAt
    storage.idGenState.set newIdState

    let sql := "INSERT INTO issues (id, title, description, design, acceptance_criteria, notes, status, priority, issue_type, assignee, estimated_minutes, created_at, updated_at, closed_at, close_reason, external_ref) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
    match ← prepare storage.db sql with
    | .error e => IO.eprintln s!"Failed to prepare insert: {e}"; return newId
    | .ok stmt =>
      bindText stmt 1 newId.value
      bindText stmt 2 issue.title
      bindText stmt 3 issue.description
      bindText stmt 4 issue.design
      bindText stmt 5 issue.acceptanceCriteria
      bindText stmt 6 issue.notes
      bindText stmt 7 issue.status.toString
      bindInt stmt 8 issue.priority.val
      bindText stmt 9 issue.issueType.toString
      Statement.bindTextOpt stmt 10 issue.assignee
      Statement.bindIntOpt stmt 11 (issue.estimatedMinutes.map (·))
      bindInt stmt 12 issue.createdAt
      bindInt stmt 13 issue.updatedAt
      Statement.bindIntOpt stmt 14 (issue.closedAt.map (·))
      bindText stmt 15 issue.closeReason
      Statement.bindTextOpt stmt 16 issue.externalRef
      discard <| step stmt
      finalize stmt
      return newId

  getIssue := fun id => do
    let sql := "SELECT id, title, description, design, acceptance_criteria, notes, status, priority, issue_type, assignee, estimated_minutes, created_at, updated_at, closed_at, close_reason, external_ref FROM issues WHERE id = ?"
    match ← prepare storage.db sql with
    | .error _ => return none
    | .ok stmt =>
      bindText stmt 1 id.value
      let rc ← step stmt
      if rc == SQLITE_ROW then
        let issue ← readIssue stmt
        finalize stmt
        -- Load labels
        let labels ← loadLabels storage.db id
        return some { issue with labels }
      else
        finalize stmt
        return none

  updateIssue := fun id f _actor => do
    -- First get the issue
    let sql := "SELECT id, title, description, design, acceptance_criteria, notes, status, priority, issue_type, assignee, estimated_minutes, created_at, updated_at, closed_at, close_reason, external_ref FROM issues WHERE id = ?"
    match ← prepare storage.db sql with
    | .error _ => return ()
    | .ok stmt =>
      bindText stmt 1 id.value
      let rc ← step stmt
      if rc == SQLITE_ROW then
        let issue ← readIssue stmt
        finalize stmt
        let updated := f issue
        -- Update in database
        let updateSql := "UPDATE issues SET title=?, description=?, design=?, acceptance_criteria=?, notes=?, status=?, priority=?, issue_type=?, assignee=?, estimated_minutes=?, updated_at=?, closed_at=?, close_reason=?, external_ref=? WHERE id=?"
        match ← prepare storage.db updateSql with
        | .error _ => return ()
        | .ok updateStmt =>
          bindText updateStmt 1 updated.title
          bindText updateStmt 2 updated.description
          bindText updateStmt 3 updated.design
          bindText updateStmt 4 updated.acceptanceCriteria
          bindText updateStmt 5 updated.notes
          bindText updateStmt 6 updated.status.toString
          bindInt updateStmt 7 updated.priority.val
          bindText updateStmt 8 updated.issueType.toString
          Statement.bindTextOpt updateStmt 9 updated.assignee
          Statement.bindIntOpt updateStmt 10 (updated.estimatedMinutes.map (·))
          bindInt updateStmt 11 updated.updatedAt
          Statement.bindIntOpt updateStmt 12 (updated.closedAt.map (·))
          bindText updateStmt 13 updated.closeReason
          Statement.bindTextOpt updateStmt 14 updated.externalRef
          bindText updateStmt 15 id.value
          discard <| step updateStmt
          finalize updateStmt
      else
        finalize stmt

  deleteIssue := fun id => do
    match ← prepare storage.db "DELETE FROM issues WHERE id = ?" with
    | .error _ => return ()
    | .ok stmt =>
      bindText stmt 1 id.value
      discard <| step stmt
      finalize stmt

  getAllIssues := do
    let sql := "SELECT id, title, description, design, acceptance_criteria, notes, status, priority, issue_type, assignee, estimated_minutes, created_at, updated_at, closed_at, close_reason, external_ref FROM issues"
    match ← prepare storage.db sql with
    | .error _ => return []
    | .ok stmt => Statement.collectRows stmt readIssue

  searchIssues := fun filter => do
    -- Build WHERE clause
    let mut conditions : List String := []
    if let some s := filter.status then
      conditions := s!"status = '{s.toString}'" :: conditions
    if let some p := filter.priority then
      conditions := s!"priority = {p.val}" :: conditions
    if let some t := filter.issueType then
      conditions := s!"issue_type = '{t.toString}'" :: conditions
    if let some a := filter.assignee then
      conditions := s!"assignee = '{a}'" :: conditions
    if !filter.titleSearch.isEmpty then
      conditions := s!"title LIKE '%{filter.titleSearch}%'" :: conditions

    let whereClause := if conditions.isEmpty then "" else " WHERE " ++ " AND ".intercalate conditions
    let sql := s!"SELECT id, title, description, design, acceptance_criteria, notes, status, priority, issue_type, assignee, estimated_minutes, created_at, updated_at, closed_at, close_reason, external_ref FROM issues{whereClause} LIMIT {filter.limit}"

    match ← prepare storage.db sql with
    | .error _ => return []
    | .ok stmt => Statement.collectRows stmt readIssue

  addDependency := fun dep _actor => do
    -- Check for cycles first using BFS
    if ← wouldCreateCycle storage.db dep.issueId dep.dependsOnId then
      return .error (.wouldCreateCycle dep.issueId dep.dependsOnId [])
    let sql := "INSERT OR IGNORE INTO dependencies (issue_id, depends_on_id, dep_type, created_at, created_by) VALUES (?, ?, ?, ?, ?)"
    match ← prepare storage.db sql with
    | .error _ => return .ok ()
    | .ok stmt =>
      bindText stmt 1 dep.issueId.value
      bindText stmt 2 dep.dependsOnId.value
      bindText stmt 3 dep.depType.toString
      bindInt stmt 4 dep.createdAt
      bindText stmt 5 dep.createdBy
      discard <| step stmt
      finalize stmt
      return .ok ()

  removeDependency := fun issueId dependsOnId _actor => do
    match ← prepare storage.db "DELETE FROM dependencies WHERE issue_id = ? AND depends_on_id = ?" with
    | .error _ => return ()
    | .ok stmt =>
      bindText stmt 1 issueId.value
      bindText stmt 2 dependsOnId.value
      discard <| step stmt
      finalize stmt

  getDependencies := fun issueId => do
    match ← prepare storage.db "SELECT issue_id, depends_on_id, dep_type, created_at, created_by FROM dependencies WHERE issue_id = ?" with
    | .error _ => return []
    | .ok stmt =>
      bindText stmt 1 issueId.value
      Statement.collectRows stmt readDependency

  getDependents := fun issueId => do
    match ← prepare storage.db "SELECT issue_id, depends_on_id, dep_type, created_at, created_by FROM dependencies WHERE depends_on_id = ?" with
    | .error _ => return []
    | .ok stmt =>
      bindText stmt 1 issueId.value
      Statement.collectRows stmt readDependency

  getAllDependencies := do
    let sql := "SELECT issue_id, depends_on_id, dep_type, created_at, created_by FROM dependencies"
    match ← prepare storage.db sql with
    | .error _ => return []
    | .ok stmt => Statement.collectRows stmt readDependency

  addLabel := fun issueId label _actor => do
    let sql := "INSERT OR IGNORE INTO labels (issue_id, label) VALUES (?, ?)"
    match ← prepare storage.db sql with
    | .error _ => return ()
    | .ok stmt =>
      bindText stmt 1 issueId.value
      bindText stmt 2 label
      discard <| step stmt
      finalize stmt

  removeLabel := fun issueId label _actor => do
    match ← prepare storage.db "DELETE FROM labels WHERE issue_id = ? AND label = ?" with
    | .error _ => return ()
    | .ok stmt =>
      bindText stmt 1 issueId.value
      bindText stmt 2 label
      discard <| step stmt
      finalize stmt

  getLabels := fun issueId => loadLabels storage.db issueId

  addComment := fun issueId text author => do
    let timestamp ← IO.monoMsNow
    match ← prepare storage.db "INSERT INTO comments (issue_id, author, text, created_at) VALUES (?, ?, ?, ?)" with
    | .error _ => return 0
    | .ok stmt =>
      bindText stmt 1 issueId.value
      bindText stmt 2 author
      bindText stmt 3 text
      bindInt stmt 4 timestamp
      discard <| step stmt
      finalize stmt
      let rowid ← lastInsertRowid storage.db
      return rowid.toNat

  getComments := fun issueId => do
    match ← prepare storage.db "SELECT id, issue_id, author, text, created_at FROM comments WHERE issue_id = ? ORDER BY created_at ASC" with
    | .error _ => return []
    | .ok stmt =>
      bindText stmt 1 issueId.value
      let mut comments : List Comment := []
      let mut running := true
      while running do
        let rc ← step stmt
        if rc == SQLITE_ROW then
          let id := (← columnInt stmt 0).toNat
          let issueIdVal ← columnText stmt 1
          let author ← columnText stmt 2
          let text ← columnText stmt 3
          let createdAt := (← columnInt stmt 4).toNat
          comments := { id, issueId := ⟨issueIdVal⟩, author, text, createdAt } :: comments
        else
          running := false
      finalize stmt
      return comments.reverse

  getAllComments := do
    let sql := "SELECT id, issue_id, author, text, created_at FROM comments ORDER BY created_at ASC"
    match ← prepare storage.db sql with
    | .error _ => return []
    | .ok stmt =>
      let mut comments : List Comment := []
      let mut running := true
      while running do
        let rc ← step stmt
        if rc == SQLITE_ROW then
          let id := (← columnInt stmt 0).toNat
          let issueIdVal ← columnText stmt 1
          let author ← columnText stmt 2
          let text ← columnText stmt 3
          let createdAt := (← columnInt stmt 4).toNat
          comments := { id, issueId := ⟨issueIdVal⟩, author, text, createdAt } :: comments
        else
          running := false
      finalize stmt
      return comments.reverse

  sendMessage := fun sender recipient subject body relatedIssue => do
    let timestamp ← IO.monoMsNow
    match ← prepare storage.db "INSERT INTO messages (from_agent, to_agent, subject, body, created_at, related_issue) VALUES (?, ?, ?, ?, ?, ?)" with
    | .error _ => return 0
    | .ok stmt =>
      bindText stmt 1 sender
      bindText stmt 2 recipient
      bindText stmt 3 subject
      bindText stmt 4 body
      bindInt stmt 5 timestamp
      Statement.bindTextOpt stmt 6 (relatedIssue.map (·.value))
      discard <| step stmt
      finalize stmt
      let rowid ← lastInsertRowid storage.db
      return rowid.toNat

  getInbox := fun recipientArg => do
    match ← prepare storage.db "SELECT id, from_agent, to_agent, subject, body, created_at, read_at, acked_at, related_issue FROM messages WHERE to_agent = ? ORDER BY created_at DESC" with
    | .error _ => return []
    | .ok stmt =>
      bindText stmt 1 recipientArg
      let mut messages : List Message := []
      let mut running := true
      while running do
        let rc ← step stmt
        if rc == SQLITE_ROW then
          let id := (← columnInt stmt 0).toNat
          let sender ← columnText stmt 1
          let recipient ← columnText stmt 2
          let subject ← columnText stmt 3
          let body ← columnText stmt 4
          let createdAt := (← columnInt stmt 5).toNat
          let readAt := let v := (← columnInt stmt 6).toNat; if v == 0 then none else some v
          let ackedAt := let v := (← columnInt stmt 7).toNat; if v == 0 then none else some v
          let relatedIssueStr ← columnText stmt 8
          let relatedIssue := if relatedIssueStr.isEmpty then none else some ⟨relatedIssueStr⟩
          messages := { id, sender, recipient, subject, body, createdAt, readAt, ackedAt, relatedIssue } :: messages
        else
          running := false
      finalize stmt
      return messages.reverse

  getMessage := fun id => do
    match ← prepare storage.db "SELECT id, from_agent, to_agent, subject, body, created_at, read_at, acked_at, related_issue FROM messages WHERE id = ?" with
    | .error _ => return none
    | .ok stmt =>
      bindInt stmt 1 id
      let rc ← step stmt
      if rc == SQLITE_ROW then
        let msgId := (← columnInt stmt 0).toNat
        let sender ← columnText stmt 1
        let recipient ← columnText stmt 2
        let subject ← columnText stmt 3
        let body ← columnText stmt 4
        let createdAt := (← columnInt stmt 5).toNat
        let readAt := let v := (← columnInt stmt 6).toNat; if v == 0 then none else some v
        let ackedAt := let v := (← columnInt stmt 7).toNat; if v == 0 then none else some v
        let relatedIssueStr ← columnText stmt 8
        let relatedIssue := if relatedIssueStr.isEmpty then none else some ⟨relatedIssueStr⟩
        finalize stmt
        return some { id := msgId, sender, recipient, subject, body, createdAt, readAt, ackedAt, relatedIssue }
      else
        finalize stmt
        return none

  markRead := fun id => do
    let timestamp ← IO.monoMsNow
    match ← prepare storage.db "UPDATE messages SET read_at = ? WHERE id = ?" with
    | .error _ => return ()
    | .ok stmt =>
      bindInt stmt 1 timestamp
      bindInt stmt 2 id
      discard <| step stmt
      finalize stmt

  markAcked := fun id => do
    let timestamp ← IO.monoMsNow
    match ← prepare storage.db "UPDATE messages SET acked_at = ? WHERE id = ?" with
    | .error _ => return ()
    | .ok stmt =>
      bindInt stmt 1 timestamp
      bindInt stmt 2 id
      discard <| step stmt
      finalize stmt

  getAllMessages := do
    let sql := "SELECT id, from_agent, to_agent, subject, body, created_at, read_at, acked_at, related_issue FROM messages ORDER BY created_at DESC"
    match ← prepare storage.db sql with
    | .error _ => return []
    | .ok stmt =>
      let mut messages : List Message := []
      let mut running := true
      while running do
        let rc ← step stmt
        if rc == SQLITE_ROW then
          let id := (← columnInt stmt 0).toNat
          let sender ← columnText stmt 1
          let recipient ← columnText stmt 2
          let subject ← columnText stmt 3
          let body ← columnText stmt 4
          let createdAt := (← columnInt stmt 5).toNat
          let readAt := let v := (← columnInt stmt 6).toNat; if v == 0 then none else some v
          let ackedAt := let v := (← columnInt stmt 7).toNat; if v == 0 then none else some v
          let relatedIssueStr ← columnText stmt 8
          let relatedIssue := if relatedIssueStr.isEmpty then none else some ⟨relatedIssueStr⟩
          messages := { id, sender, recipient, subject, body, createdAt, readAt, ackedAt, relatedIssue } :: messages
        else
          running := false
      finalize stmt
      return messages.reverse

  getReadyWork := fun filter => do
    -- Get blocked issue IDs
    let blockedSql := "SELECT DISTINCT d.issue_id FROM dependencies d JOIN issues i ON d.depends_on_id = i.id WHERE d.dep_type IN ('blocks', 'parent-child') AND i.status != 'closed'"
    let mut blockedIds : List String := []
    match ← prepare storage.db blockedSql with
    | .error _ => pure ()
    | .ok stmt =>
      let mut running := true
      while running do
        let rc ← step stmt
        if rc == SQLITE_ROW then
          blockedIds := (← columnText stmt 0) :: blockedIds
        else
          running := false
      finalize stmt

    -- Get ready issues
    let mut conditions : List String := ["status IN ('open', 'in_progress')"]
    if let some p := filter.priority then
      conditions := s!"priority = {p.val}" :: conditions
    if let some a := filter.assignee then
      conditions := s!"assignee = '{a}'" :: conditions
    if filter.unassigned then
      conditions := "assignee IS NULL" :: conditions

    let whereClause := " WHERE " ++ " AND ".intercalate conditions
    let sql := s!"SELECT id, title, description, design, acceptance_criteria, notes, status, priority, issue_type, assignee, estimated_minutes, created_at, updated_at, closed_at, close_reason, external_ref FROM issues{whereClause} LIMIT {filter.limit}"

    match ← prepare storage.db sql with
    | .error _ => return []
    | .ok stmt =>
      let mut issues : List Issue := []
      let mut running := true
      while running do
        let rc ← step stmt
        if rc == SQLITE_ROW then
          let issue ← readIssue stmt
          -- Filter out blocked
          if !blockedIds.contains issue.id.value then
            issues := issue :: issues
        else
          running := false
      finalize stmt
      return issues.take filter.limit

  getBlockedIssues := do
    let sql := "SELECT DISTINCT i.id, i.title, i.description, i.design, i.acceptance_criteria, i.notes, i.status, i.priority, i.issue_type, i.assignee, i.estimated_minutes, i.created_at, i.updated_at, i.closed_at, i.close_reason, i.external_ref FROM issues i JOIN dependencies d ON i.id = d.issue_id JOIN issues blocker ON d.depends_on_id = blocker.id WHERE d.dep_type IN ('blocks', 'parent-child') AND blocker.status != 'closed' AND i.status IN ('open', 'in_progress')"
    match ← prepare storage.db sql with
    | .error _ => return []
    | .ok stmt =>
      let mut blocked : List BlockedIssue := []
      let mut running := true
      while running do
        let rc ← step stmt
        if rc == SQLITE_ROW then
          let issue ← readIssue stmt
          -- Get blockers for this issue
          let blockers ← getBlockersFor storage.db issue.id
          blocked := { issue, blockedByCount := blockers.length, blockedBy := blockers } :: blocked
        else
          running := false
      finalize stmt
      return blocked

  wouldCreateCycle := fun from_ to => wouldCreateCycle storage.db from_ to

  detectCycles := do
    -- For now, return empty (we prevent cycles on add)
    return []

  save := do
    -- SQLite auto-saves, nothing to do
    return ()

  load := do
    -- Data is already in SQLite, nothing to do
    return ()
}
where
  loadLabels (db : Database) (issueId : IssueId) : IO (List String) := do
    match ← prepare db "SELECT label FROM labels WHERE issue_id = ?" with
    | .error _ => return []
    | .ok stmt =>
      bindText stmt 1 issueId.value
      Statement.collectRows stmt (columnText · 0)

  getBlockersFor (db : Database) (issueId : IssueId) : IO (List IssueId) := do
    match ← prepare db "SELECT d.depends_on_id FROM dependencies d JOIN issues i ON d.depends_on_id = i.id WHERE d.issue_id = ? AND d.dep_type IN ('blocks', 'parent-child') AND i.status != 'closed'" with
    | .error _ => return []
    | .ok stmt =>
      bindText stmt 1 issueId.value
      Statement.collectRows stmt (fun s => return ⟨← columnText s 0⟩)

  wouldCreateCycle (db : Database) (from_ to : IssueId) : IO Bool := do
    -- BFS from 'to' to see if we can reach 'from_'
    let mut queue : List IssueId := [to]
    let mut visited : Std.HashSet IssueId := {}
    let mut fuel := 1000

    -- Prepare the statement once and reuse
    match ← prepare db "SELECT depends_on_id FROM dependencies WHERE issue_id = ?" with
    | .error _ => return false
    | .ok stmt =>
      while !queue.isEmpty && fuel > 0 do
        fuel := fuel - 1
        match queue with
        | [] => break
        | current :: rest =>
          queue := rest
          if current == from_ then
            finalize stmt
            return true
          if visited.contains current then
            continue
          visited := visited.insert current
          -- Get dependencies of current
          reset stmt
          bindText stmt 1 current.value
          let mut running := true
          while running do
            let rc ← step stmt
            if rc == SQLITE_ROW then
              let depId ← columnText stmt 0
              queue := ⟨depId⟩ :: queue
            else
              running := false
      finalize stmt
      return false

/-- Open or create SQLite storage -/
def openOrCreate (path : System.FilePath := ".beads/beads.db") : IO (Except String SqliteStorage) := do
  -- Ensure parent directory exists
  if let some parent := path.parent then
    IO.FS.createDirAll parent
  create path

end SqliteStorage

end Beads
