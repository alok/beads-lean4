/-
Beads Issue Templates

Predefined templates for common issue patterns (bug reports, feature requests, etc.)
Templates are stored as YAML-like files in .beads/templates/
-/
import Lean.Data.Json
import Beads.Core

namespace Beads.Sync.Templates

open Lean Json
open Beads

/-- Template definition -/
structure Template where
  name : String
  description : String
  titlePrefix : String := ""
  defaultType : IssueType := .task
  defaultPriority : Priority := Priority.default
  defaultLabels : List String := []
  bodyTemplate : String := ""  -- Template for description
  deriving Repr, Inhabited

instance : ToJson Template where
  toJson t := Json.mkObj [
    ("name", Json.str t.name),
    ("description", Json.str t.description),
    ("title_prefix", Json.str t.titlePrefix),
    ("default_type", Json.str t.defaultType.toString),
    ("default_priority", Json.num t.defaultPriority.val),
    ("default_labels", Json.arr (t.defaultLabels.map Json.str).toArray),
    ("body_template", Json.str t.bodyTemplate)
  ]

instance : FromJson Template where
  fromJson? j := do
    let name ← j.getObjValAs? String "name"
    let description := (j.getObjVal? "description").toOption.bind (·.getStr?.toOption) |>.getD ""
    let titlePrefix := (j.getObjVal? "title_prefix").toOption.bind (·.getStr?.toOption) |>.getD ""
    let defaultType := (j.getObjVal? "default_type").toOption.bind (·.getStr?.toOption)
      |>.bind IssueType.fromString? |>.getD .task
    let defaultPriority := (j.getObjVal? "default_priority").toOption.bind (·.getNat?.toOption)
      |>.bind Priority.fromNat? |>.getD Priority.default
    let defaultLabels := (j.getObjVal? "default_labels").toOption.bind (·.getArr?.toOption)
      |>.getD #[] |>.toList.filterMap (·.getStr?.toOption)
    let bodyTemplate := (j.getObjVal? "body_template").toOption.bind (·.getStr?.toOption) |>.getD ""
    pure { name, description, titlePrefix, defaultType, defaultPriority, defaultLabels, bodyTemplate }

/-- Template manager -/
structure TemplateManager where
  templatesDir : System.FilePath
  deriving Inhabited

/-- Create template manager for a beads directory -/
def TemplateManager.forDirectory (beadsDir : System.FilePath) : TemplateManager :=
  { templatesDir := beadsDir / "templates" }

/-- Ensure templates directory exists -/
def TemplateManager.ensureDir (tm : TemplateManager) : IO Unit := do
  if !(← tm.templatesDir.pathExists) then
    IO.FS.createDirAll tm.templatesDir

/-- List available templates -/
def TemplateManager.list (tm : TemplateManager) : IO (List Template) := do
  if !(← tm.templatesDir.pathExists) then
    return []

  let entries ← tm.templatesDir.readDir
  let mut templates : List Template := []
  for entry in entries do
    if entry.fileName.endsWith ".json" then
      let content ← IO.FS.readFile entry.path
      match Json.parse content >>= fromJson? with
      | .ok template => templates := template :: templates
      | .error _ => pure ()  -- Skip invalid templates
  return templates.reverse

/-- Get a specific template by name -/
def TemplateManager.get (tm : TemplateManager) (name : String) : IO (Option Template) := do
  let path := tm.templatesDir / s!"{name}.json"
  if !(← path.pathExists) then
    return none

  let content ← IO.FS.readFile path
  match Json.parse content >>= fromJson? with
  | .ok template => return some template
  | .error _ => return none

/-- Save a template -/
def TemplateManager.save (tm : TemplateManager) (template : Template) : IO Unit := do
  tm.ensureDir
  let path := tm.templatesDir / s!"{template.name}.json"
  IO.FS.writeFile path ((toJson template).pretty)

/-- Delete a template -/
def TemplateManager.delete (tm : TemplateManager) (name : String) : IO Bool := do
  let path := tm.templatesDir / s!"{name}.json"
  if ← path.pathExists then
    IO.FS.removeFile path
    return true
  return false

/-- Create built-in templates -/
def TemplateManager.createBuiltins (tm : TemplateManager) : IO Unit := do
  tm.ensureDir

  -- Bug report template
  let bug : Template := {
    name := "bug"
    description := "Bug report template"
    titlePrefix := "[Bug] "
    defaultType := .bug
    defaultPriority := ⟨1, by omega⟩  -- P1 for bugs
    defaultLabels := ["bug"]
    bodyTemplate := "## Description\nDescribe the bug...\n\n## Steps to Reproduce\n1. \n2. \n3. \n\n## Expected Behavior\n\n## Actual Behavior\n\n## Environment\n"
  }
  tm.save bug

  -- Feature request template
  let feature : Template := {
    name := "feature"
    description := "Feature request template"
    titlePrefix := "[Feature] "
    defaultType := .feature
    defaultPriority := Priority.default
    defaultLabels := ["feature", "enhancement"]
    bodyTemplate := "## Summary\nBrief description of the feature...\n\n## Motivation\nWhy is this needed?\n\n## Proposed Solution\n\n## Alternatives Considered\n"
  }
  tm.save feature

  -- Epic template
  let epic : Template := {
    name := "epic"
    description := "Epic (large feature) template"
    titlePrefix := "[Epic] "
    defaultType := .epic
    defaultPriority := ⟨1, by omega⟩
    defaultLabels := ["epic"]
    bodyTemplate := "## Overview\nHigh-level description...\n\n## Goals\n- [ ] Goal 1\n- [ ] Goal 2\n\n## Sub-tasks\nWill be linked as child issues\n\n## Success Criteria\n"
  }
  tm.save epic

  -- Chore template
  let chore : Template := {
    name := "chore"
    description := "Maintenance/chore task template"
    titlePrefix := ""
    defaultType := .task
    defaultPriority := ⟨3, by omega⟩  -- P3 for chores
    defaultLabels := ["chore"]
    bodyTemplate := ""
  }
  tm.save chore

end Beads.Sync.Templates
