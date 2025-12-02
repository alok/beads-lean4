import Lake
open Lake DSL
open System (FilePath)

require verso from git "https://github.com/leanprover/verso"@"main"

package beads where
  precompileModules := true
  moreLinkArgs := #["-L/opt/homebrew/opt/sqlite/lib", "-lsqlite3"]

-- SQLite FFI build function
def sqliteFFIOTarget (pkg : Package) : FetchM (Job FilePath) := do
  let oFile := pkg.dir / defaultBuildDir / "ffi" / "sqlite_ffi.o"
  let srcTarget ← inputTextFile <| pkg.dir / "ffi" / "sqlite_ffi.c"
  buildFileAfterDep oFile srcTarget fun srcFile => do
    let flags := #["-I", (← getLeanIncludeDir).toString,
                   "-I", "/opt/homebrew/opt/sqlite/include",
                   "-fPIC", "-O2"]
    compileO oFile srcFile flags

-- SQLite FFI static library
extern_lib sqlite_ffi (pkg) := do
  let name := nameToStaticLib "sqlite_ffi"
  let oTarget ← sqliteFFIOTarget pkg
  buildStaticLib (pkg.staticLibDir / name) #[oTarget]

-- Core library
@[default_target]
lean_lib Beads where
  roots := #[`Beads]

-- CLI executable
@[default_target]
lean_exe «beads» where
  root := `BeadsCLI.Main
  supportInterpreter := true

-- Documentation (Verso-based)
lean_lib BeadsDocs where
  srcDir := "docs"
  roots := #[`Manual]

-- Documentation generator executable
lean_exe «beads-docs» where
  srcDir := "docs"
  root := `Main
  supportInterpreter := true

-- Property-based tests using Plausible
lean_lib BeadsTest where
  roots := #[`BeadsTest.Arbitrary, `BeadsTest.IdGeneration, `BeadsTest.TypeInvariants, `BeadsTest.JsonRoundtrip, `BeadsTest.CycleDetection]

-- Test executable
lean_exe «beads-test» where
  root := `BeadsTest.Main
  supportInterpreter := true
