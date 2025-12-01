/-
SQLite FFI Bindings for Lean 4
Low-level bindings to sqlite3 C library
-/

namespace Beads.Sqlite

/-- Opaque type for sqlite3 database handle -/
opaque DatabasePointed : NonemptyType
def Database : Type := DatabasePointed.type
instance : Nonempty Database := DatabasePointed.property

/-- Opaque type for sqlite3 prepared statement -/
opaque StatementPointed : NonemptyType
def Statement : Type := StatementPointed.type
instance : Nonempty Statement := StatementPointed.property

/-- SQLite result codes -/
def SQLITE_OK : Nat := 0
def SQLITE_ROW : Nat := 100
def SQLITE_DONE : Nat := 101

/-- SQLite column types -/
def SQLITE_INTEGER : Nat := 1
def SQLITE_FLOAT : Nat := 2
def SQLITE_TEXT : Nat := 3
def SQLITE_BLOB : Nat := 4
def SQLITE_NULL : Nat := 5

/-- Open a database file -/
@[extern "lean_sqlite3_open_v2"]
opaque open' : String → IO (Except String Database)

/-- Close a database connection -/
@[extern "lean_sqlite3_close"]
opaque close : @& Database → IO Unit

/-- Execute SQL without results -/
@[extern "lean_sqlite3_exec"]
opaque exec : @& Database → String → IO (Except String Unit)

/-- Prepare a SQL statement -/
@[extern "lean_sqlite3_prepare"]
opaque prepare : @& Database → String → IO (Except String Statement)

/-- Step through a statement (returns result code) -/
@[extern "lean_sqlite3_step"]
opaque step : @& Statement → IO Nat

/-- Reset a statement for reuse -/
@[extern "lean_sqlite3_reset"]
opaque reset : @& Statement → IO Unit

/-- Finalize a statement -/
@[extern "lean_sqlite3_finalize"]
opaque finalize : @& Statement → IO Unit

/-- Bind text parameter (1-indexed) -/
@[extern "lean_sqlite3_bind_text"]
opaque bindText : @& Statement → Nat → String → IO Unit

/-- Bind integer parameter (1-indexed) -/
@[extern "lean_sqlite3_bind_int"]
opaque bindInt : @& Statement → Nat → Int → IO Unit

/-- Bind null parameter (1-indexed) -/
@[extern "lean_sqlite3_bind_null"]
opaque bindNull : @& Statement → Nat → IO Unit

/-- Get column count -/
@[extern "lean_sqlite3_column_count"]
opaque columnCount : @& Statement → IO Nat

/-- Get column as text (0-indexed) -/
@[extern "lean_sqlite3_column_text"]
opaque columnText : @& Statement → Nat → IO String

/-- Get column as integer (0-indexed) -/
@[extern "lean_sqlite3_column_int"]
opaque columnInt : @& Statement → Nat → IO Int

/-- Get column type (0-indexed) -/
@[extern "lean_sqlite3_column_type"]
opaque columnType : @& Statement → Nat → IO Nat

/-- Check if column is null (0-indexed) -/
@[extern "lean_sqlite3_column_is_null"]
opaque columnIsNull : @& Statement → Nat → IO Bool

/-- Get last insert rowid -/
@[extern "lean_sqlite3_last_insert_rowid"]
opaque lastInsertRowid : @& Database → IO Int

/-- Get number of changed rows -/
@[extern "lean_sqlite3_changes"]
opaque changes : @& Database → IO Nat

/-- Get error message -/
@[extern "lean_sqlite3_errmsg"]
opaque errmsg : @& Database → IO String

namespace Database

/-- Open a database, creating if necessary -/
def openOrCreate (path : String) : IO (Except String Database) := open' path

/-- Execute multiple SQL statements -/
def execMany (db : Database) (statements : List String) : IO (Except String Unit) := do
  for sql in statements do
    match ← exec db sql with
    | .error e => return .error e
    | .ok () => pure ()
  return .ok ()

end Database

namespace Statement

/-- Execute a statement and discard results -/
def execute (stmt : Statement) : IO (Except String Unit) := do
  let rc ← step stmt
  reset stmt
  if rc == SQLITE_DONE || rc == SQLITE_ROW then
    return .ok ()
  else
    return .error s!"SQLite step failed with code {rc}"

/-- Bind text, handling Option -/
def bindTextOpt (stmt : Statement) (idx : Nat) (val : Option String) : IO Unit := do
  match val with
  | some s => bindText stmt idx s
  | none => bindNull stmt idx

/-- Bind int, handling Option -/
def bindIntOpt (stmt : Statement) (idx : Nat) (val : Option Int) : IO Unit := do
  match val with
  | some n => bindInt stmt idx n
  | none => bindNull stmt idx

/-- Get optional text column -/
def columnTextOpt (stmt : Statement) (idx : Nat) : IO (Option String) := do
  if ← columnIsNull stmt idx then
    return none
  else
    return some (← columnText stmt idx)

/-- Get optional int column -/
def columnIntOpt (stmt : Statement) (idx : Nat) : IO (Option Int) := do
  if ← columnIsNull stmt idx then
    return none
  else
    return some (← columnInt stmt idx)

/-- Collect all rows from a statement using a row reader function -/
def collectRows (stmt : Statement) (readRow : Statement → IO α) : IO (List α) := do
  let mut rows : List α := []
  let mut running := true
  while running do
    let rc ← step stmt
    if rc == SQLITE_ROW then
      let row ← readRow stmt
      rows := row :: rows
    else
      running := false
  finalize stmt
  return rows.reverse

/-- Collect all rows, with finalization handled separately -/
def collectRowsNoFinalize (stmt : Statement) (readRow : Statement → IO α) : IO (List α) := do
  let mut rows : List α := []
  let mut running := true
  while running do
    let rc ← step stmt
    if rc == SQLITE_ROW then
      let row ← readRow stmt
      rows := row :: rows
    else
      running := false
  return rows.reverse

end Statement

end Beads.Sqlite
