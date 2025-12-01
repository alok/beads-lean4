/*
 * SQLite FFI bindings for Lean 4
 * Minimal wrapper around sqlite3 API
 */

#include <lean/lean.h>
#include <sqlite3.h>
#include <string.h>
#include <stdlib.h>

/* ========== Database Handle ========== */

/* Finalize sqlite3 database handle */
static void sqlite3_finalizer(void* ptr) {
    if (ptr != NULL) {
        sqlite3_close((sqlite3*)ptr);
    }
}

/* External class for sqlite3* */
static lean_external_class* g_sqlite3_class = NULL;

lean_external_class* get_sqlite3_class() {
    if (g_sqlite3_class == NULL) {
        g_sqlite3_class = lean_register_external_class(sqlite3_finalizer, NULL);
    }
    return g_sqlite3_class;
}

/* Wrap sqlite3* in Lean object */
static lean_object* sqlite3_to_lean(sqlite3* db) {
    return lean_alloc_external(get_sqlite3_class(), db);
}

/* Unwrap sqlite3* from Lean object */
static sqlite3* lean_to_sqlite3(lean_object* obj) {
    return (sqlite3*)lean_get_external_data(obj);
}

/* ========== Statement Handle ========== */

/* Finalize sqlite3_stmt handle */
static void sqlite3_stmt_finalizer(void* ptr) {
    if (ptr != NULL) {
        sqlite3_finalize((sqlite3_stmt*)ptr);
    }
}

/* External class for sqlite3_stmt* */
static lean_external_class* g_sqlite3_stmt_class = NULL;

lean_external_class* get_sqlite3_stmt_class() {
    if (g_sqlite3_stmt_class == NULL) {
        g_sqlite3_stmt_class = lean_register_external_class(sqlite3_stmt_finalizer, NULL);
    }
    return g_sqlite3_stmt_class;
}

/* Wrap sqlite3_stmt* in Lean object */
static lean_object* sqlite3_stmt_to_lean(sqlite3_stmt* stmt) {
    return lean_alloc_external(get_sqlite3_stmt_class(), stmt);
}

/* Unwrap sqlite3_stmt* from Lean object */
static sqlite3_stmt* lean_to_sqlite3_stmt(lean_object* obj) {
    return (sqlite3_stmt*)lean_get_external_data(obj);
}

/* ========== Database Operations ========== */

/* Open database: String -> IO (Except String Database) */
LEAN_EXPORT lean_object* lean_sqlite3_open_v2(lean_object* path, lean_object* w) {
    const char* path_str = lean_string_cstr(path);
    sqlite3* db = NULL;

    int rc = sqlite3_open(path_str, &db);

    lean_object* result;
    if (rc != SQLITE_OK) {
        const char* err = sqlite3_errmsg(db);
        lean_object* err_str = lean_mk_string(err ? err : "Unknown error");
        sqlite3_close(db);
        // Except.error (constructor index 0)
        result = lean_alloc_ctor(0, 1, 0);
        lean_ctor_set(result, 0, err_str);
    } else {
        lean_object* db_obj = sqlite3_to_lean(db);
        // Except.ok (constructor index 1)
        result = lean_alloc_ctor(1, 1, 0);
        lean_ctor_set(result, 0, db_obj);
    }

    return lean_io_result_mk_ok(result);
}

/* Close database: Database -> IO Unit */
LEAN_EXPORT lean_object* lean_sqlite3_close(lean_object* db_obj, lean_object* w) {
    /* The finalizer will handle closing, just decrement reference */
    return lean_io_result_mk_ok(lean_box(0));
}

/* Execute SQL without results: Database -> String -> IO (Except String Unit) */
LEAN_EXPORT lean_object* lean_sqlite3_exec(lean_object* db_obj, lean_object* sql, lean_object* w) {
    sqlite3* db = lean_to_sqlite3(db_obj);
    const char* sql_str = lean_string_cstr(sql);
    char* err_msg = NULL;

    int rc = sqlite3_exec(db, sql_str, NULL, NULL, &err_msg);

    lean_object* result;
    if (rc != SQLITE_OK) {
        lean_object* err_str = lean_mk_string(err_msg ? err_msg : "Unknown error");
        sqlite3_free(err_msg);
        // Except.error (constructor index 0)
        result = lean_alloc_ctor(0, 1, 0);
        lean_ctor_set(result, 0, err_str);
    } else {
        // Except.ok Unit (constructor index 1)
        result = lean_alloc_ctor(1, 1, 0);
        lean_ctor_set(result, 0, lean_box(0));
    }

    return lean_io_result_mk_ok(result);
}

/* ========== Prepared Statement Operations ========== */

/* Prepare statement: Database -> String -> IO (Except String Statement) */
LEAN_EXPORT lean_object* lean_sqlite3_prepare(lean_object* db_obj, lean_object* sql, lean_object* w) {
    sqlite3* db = lean_to_sqlite3(db_obj);
    const char* sql_str = lean_string_cstr(sql);
    sqlite3_stmt* stmt = NULL;

    int rc = sqlite3_prepare_v2(db, sql_str, -1, &stmt, NULL);

    lean_object* result;
    if (rc != SQLITE_OK) {
        const char* err = sqlite3_errmsg(db);
        lean_object* err_str = lean_mk_string(err ? err : "Unknown error");
        // Except.error (constructor index 0)
        result = lean_alloc_ctor(0, 1, 0);
        lean_ctor_set(result, 0, err_str);
    } else {
        lean_object* stmt_obj = sqlite3_stmt_to_lean(stmt);
        // Except.ok (constructor index 1)
        result = lean_alloc_ctor(1, 1, 0);
        lean_ctor_set(result, 0, stmt_obj);
    }

    return lean_io_result_mk_ok(result);
}

/* Step statement: Statement -> IO Nat (SQLITE_ROW=100, SQLITE_DONE=101, etc) */
LEAN_EXPORT lean_object* lean_sqlite3_step(lean_object* stmt_obj, lean_object* w) {
    sqlite3_stmt* stmt = lean_to_sqlite3_stmt(stmt_obj);
    int rc = sqlite3_step(stmt);
    return lean_io_result_mk_ok(lean_unsigned_to_nat(rc));
}

/* Reset statement: Statement -> IO Unit */
LEAN_EXPORT lean_object* lean_sqlite3_reset(lean_object* stmt_obj, lean_object* w) {
    sqlite3_stmt* stmt = lean_to_sqlite3_stmt(stmt_obj);
    sqlite3_reset(stmt);
    return lean_io_result_mk_ok(lean_box(0));
}

/* Finalize statement: Statement -> IO Unit */
LEAN_EXPORT lean_object* lean_sqlite3_finalize(lean_object* stmt_obj, lean_object* w) {
    /* The finalizer will handle this */
    return lean_io_result_mk_ok(lean_box(0));
}

/* ========== Binding Parameters ========== */

/* Bind text: Statement -> Nat -> String -> IO Unit */
LEAN_EXPORT lean_object* lean_sqlite3_bind_text(lean_object* stmt_obj, lean_object* idx, lean_object* text, lean_object* w) {
    sqlite3_stmt* stmt = lean_to_sqlite3_stmt(stmt_obj);
    size_t idx_val = lean_usize_of_nat(idx);
    const char* text_str = lean_string_cstr(text);

    sqlite3_bind_text(stmt, (int)idx_val, text_str, -1, SQLITE_TRANSIENT);
    return lean_io_result_mk_ok(lean_box(0));
}

/* Bind int: Statement -> Nat -> Int -> IO Unit */
LEAN_EXPORT lean_object* lean_sqlite3_bind_int(lean_object* stmt_obj, lean_object* idx, lean_object* val, lean_object* w) {
    sqlite3_stmt* stmt = lean_to_sqlite3_stmt(stmt_obj);
    size_t idx_val = lean_usize_of_nat(idx);

    /* Handle both boxed and unboxed integers */
    int64_t int_val;
    if (lean_is_scalar(val)) {
        int_val = lean_unbox(val);
    } else {
        int_val = lean_int64_of_int(val);
    }

    sqlite3_bind_int64(stmt, (int)idx_val, int_val);
    return lean_io_result_mk_ok(lean_box(0));
}

/* Bind null: Statement -> Nat -> IO Unit */
LEAN_EXPORT lean_object* lean_sqlite3_bind_null(lean_object* stmt_obj, lean_object* idx, lean_object* w) {
    sqlite3_stmt* stmt = lean_to_sqlite3_stmt(stmt_obj);
    size_t idx_val = lean_usize_of_nat(idx);

    sqlite3_bind_null(stmt, (int)idx_val);
    return lean_io_result_mk_ok(lean_box(0));
}

/* ========== Column Access ========== */

/* Get column count: Statement -> IO Nat */
LEAN_EXPORT lean_object* lean_sqlite3_column_count(lean_object* stmt_obj, lean_object* w) {
    sqlite3_stmt* stmt = lean_to_sqlite3_stmt(stmt_obj);
    int count = sqlite3_column_count(stmt);
    return lean_io_result_mk_ok(lean_unsigned_to_nat(count));
}

/* Get column text: Statement -> Nat -> IO String */
LEAN_EXPORT lean_object* lean_sqlite3_column_text(lean_object* stmt_obj, lean_object* idx, lean_object* w) {
    sqlite3_stmt* stmt = lean_to_sqlite3_stmt(stmt_obj);
    size_t idx_val = lean_usize_of_nat(idx);

    const unsigned char* text = sqlite3_column_text(stmt, (int)idx_val);
    lean_object* result = lean_mk_string(text ? (const char*)text : "");
    return lean_io_result_mk_ok(result);
}

/* Get column int: Statement -> Nat -> IO Int */
LEAN_EXPORT lean_object* lean_sqlite3_column_int(lean_object* stmt_obj, lean_object* idx, lean_object* w) {
    sqlite3_stmt* stmt = lean_to_sqlite3_stmt(stmt_obj);
    size_t idx_val = lean_usize_of_nat(idx);

    int64_t val = sqlite3_column_int64(stmt, (int)idx_val);
    return lean_io_result_mk_ok(lean_int64_to_int(val));
}

/* Get column type: Statement -> Nat -> IO Nat */
LEAN_EXPORT lean_object* lean_sqlite3_column_type(lean_object* stmt_obj, lean_object* idx, lean_object* w) {
    sqlite3_stmt* stmt = lean_to_sqlite3_stmt(stmt_obj);
    size_t idx_val = lean_usize_of_nat(idx);

    int type = sqlite3_column_type(stmt, (int)idx_val);
    return lean_io_result_mk_ok(lean_unsigned_to_nat(type));
}

/* Check if column is null: Statement -> Nat -> IO Bool */
LEAN_EXPORT lean_object* lean_sqlite3_column_is_null(lean_object* stmt_obj, lean_object* idx, lean_object* w) {
    sqlite3_stmt* stmt = lean_to_sqlite3_stmt(stmt_obj);
    size_t idx_val = lean_usize_of_nat(idx);

    int type = sqlite3_column_type(stmt, (int)idx_val);
    return lean_io_result_mk_ok(lean_box(type == SQLITE_NULL ? 1 : 0));
}

/* ========== Utility ========== */

/* Get last insert rowid: Database -> IO Int */
LEAN_EXPORT lean_object* lean_sqlite3_last_insert_rowid(lean_object* db_obj, lean_object* w) {
    sqlite3* db = lean_to_sqlite3(db_obj);
    int64_t rowid = sqlite3_last_insert_rowid(db);
    return lean_io_result_mk_ok(lean_int64_to_int(rowid));
}

/* Get changes count: Database -> IO Nat */
LEAN_EXPORT lean_object* lean_sqlite3_changes(lean_object* db_obj, lean_object* w) {
    sqlite3* db = lean_to_sqlite3(db_obj);
    int changes = sqlite3_changes(db);
    return lean_io_result_mk_ok(lean_unsigned_to_nat(changes));
}

/* Get error message: Database -> IO String */
LEAN_EXPORT lean_object* lean_sqlite3_errmsg(lean_object* db_obj, lean_object* w) {
    sqlite3* db = lean_to_sqlite3(db_obj);
    const char* err = sqlite3_errmsg(db);
    return lean_io_result_mk_ok(lean_mk_string(err ? err : ""));
}

/* Constants */
LEAN_EXPORT lean_object* lean_sqlite3_row(lean_object* w) {
    return lean_io_result_mk_ok(lean_box(SQLITE_ROW));
}

LEAN_EXPORT lean_object* lean_sqlite3_done(lean_object* w) {
    return lean_io_result_mk_ok(lean_box(SQLITE_DONE));
}

LEAN_EXPORT lean_object* lean_sqlite3_ok(lean_object* w) {
    return lean_io_result_mk_ok(lean_box(SQLITE_OK));
}
