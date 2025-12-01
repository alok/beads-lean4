import Lake
open Lake DSL

require verso from git "https://github.com/leanprover/verso"@"main"

package beads where
  precompileModules := true

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
