/-
Beads JSON Serialization
Implements ToJson and FromJson for core types
-/
import Beads.Core.Issue
import Lean.Data.Json

namespace Beads

open Lean Json

-- Status JSON
instance : ToJson Status where
  toJson s := .str s.toString

instance : FromJson Status where
  fromJson? j := do
    let s ← j.getStr?
    match Status.fromString? s with
    | some status => pure status
    | none => throw s!"Invalid status: {s}"

-- IssueType JSON
instance : ToJson IssueType where
  toJson t := .str t.toString

instance : FromJson IssueType where
  fromJson? j := do
    let s ← j.getStr?
    match IssueType.fromString? s with
    | some it => pure it
    | none => throw s!"Invalid issue type: {s}"

-- DependencyType JSON
instance : ToJson DependencyType where
  toJson d := .str d.toString

instance : FromJson DependencyType where
  fromJson? j := do
    let s ← j.getStr?
    match DependencyType.fromString? s with
    | some dt => pure dt
    | none => throw s!"Invalid dependency type: {s}"

-- Priority JSON
instance : ToJson Priority where
  toJson p := .num p.val

instance : FromJson Priority where
  fromJson? j := do
    let n ← j.getNat?
    match Priority.fromNat? n with
    | some p => pure p
    | none => throw s!"Invalid priority: {n} (must be 0-4)"

-- IssueId JSON
instance : ToJson IssueId where
  toJson id := .str id.value

instance : FromJson IssueId where
  fromJson? j := do
    let s ← j.getStr?
    pure ⟨s⟩

-- Helper to get optional string from JSON object
def getOptStr (j : Json) (key : String) : Option String :=
  match j.getObjValAs? String key with
  | .ok s => some s
  | .error _ => none

-- Helper to get optional Nat from JSON object
def getOptNat (j : Json) (key : String) : Option Nat :=
  match j.getObjValAs? Nat key with
  | .ok n => some n
  | .error _ => none

-- Issue JSON (matches Go's JSON tags)
instance : ToJson Issue where
  toJson i := .mkObj [
    ("id", toJson i.id),
    ("title", .str i.title),
    ("description", .str i.description),
    ("design", if i.design.isEmpty then .null else .str i.design),
    ("acceptance_criteria", if i.acceptanceCriteria.isEmpty then .null else .str i.acceptanceCriteria),
    ("notes", if i.notes.isEmpty then .null else .str i.notes),
    ("status", toJson i.status),
    ("priority", toJson i.priority),
    ("issue_type", toJson i.issueType),
    ("assignee", match i.assignee with | some a => .str a | none => .null),
    ("estimated_minutes", match i.estimatedMinutes with | some m => .num m | none => .null),
    ("created_at", .num i.createdAt),
    ("updated_at", .num i.updatedAt),
    ("closed_at", match i.closedAt with | some t => .num t | none => .null),
    ("close_reason", if i.closeReason.isEmpty then .null else .str i.closeReason),
    ("external_ref", match i.externalRef with | some r => .str r | none => .null),
    ("labels", if i.labels.isEmpty then .null else .arr (i.labels.map .str).toArray)
  ]

instance : FromJson Issue where
  fromJson? j := do
    let id ← j.getObjValAs? String "id"
    let title ← j.getObjValAs? String "title"
    let description := (getOptStr j "description").getD ""
    let design := (getOptStr j "design").getD ""
    let acceptanceCriteria := (getOptStr j "acceptance_criteria").getD ""
    let notes := (getOptStr j "notes").getD ""
    let status ← j.getObjValAs? Status "status"
    let priority ← j.getObjValAs? Priority "priority"
    let issueType ← j.getObjValAs? IssueType "issue_type"
    let assignee := getOptStr j "assignee"
    let estimatedMinutes := getOptNat j "estimated_minutes"
    let createdAt ← j.getObjValAs? Nat "created_at"
    let updatedAt ← j.getObjValAs? Nat "updated_at"
    let closedAt := getOptNat j "closed_at"
    let closeReason := (getOptStr j "close_reason").getD ""
    let externalRef := getOptStr j "external_ref"
    let labels : List String :=
      let labelsJson := j.getObjVal? "labels"
      match labelsJson with
      | .ok jsonVal =>
        let arrResult := jsonVal.getArr?
        match arrResult with
        | .ok labelArr => labelArr.toList.filterMap (fun x => x.getStr?.toOption)
        | .error _ => []
      | .error _ => []
    pure {
      id := ⟨id⟩
      title := title
      description := description
      design := design
      acceptanceCriteria := acceptanceCriteria
      notes := notes
      status := status
      priority := priority
      issueType := issueType
      assignee := assignee
      estimatedMinutes := estimatedMinutes
      createdAt := createdAt
      updatedAt := updatedAt
      closedAt := closedAt
      closeReason := closeReason
      externalRef := externalRef
      labels := labels
    }

-- Dependency JSON
instance : ToJson Dependency where
  toJson d := .mkObj [
    ("issue_id", toJson d.issueId),
    ("depends_on_id", toJson d.dependsOnId),
    ("type", toJson d.depType),
    ("created_at", .num d.createdAt),
    ("created_by", .str d.createdBy)
  ]

instance : FromJson Dependency where
  fromJson? j := do
    let issueId ← j.getObjValAs? String "issue_id"
    let dependsOnId ← j.getObjValAs? String "depends_on_id"
    let depType ← j.getObjValAs? DependencyType "type"
    let createdAt ← j.getObjValAs? Nat "created_at"
    let createdBy ← j.getObjValAs? String "created_by"
    pure {
      issueId := ⟨issueId⟩
      dependsOnId := ⟨dependsOnId⟩
      depType := depType
      createdAt := createdAt
      createdBy := createdBy
    }

-- BlockedIssue JSON (for CLI output)
instance : ToJson BlockedIssue where
  toJson b :=
    let issueJson := toJson b.issue
    match issueJson with
    | .obj fields =>
      .obj (fields.insert "blocked_by_count" (.num b.blockedByCount)
                  |>.insert "blocked_by" (.arr (b.blockedBy.map (toJson ·)).toArray))
    | _ => issueJson

-- Statistics JSON
instance : ToJson Statistics where
  toJson s := .mkObj [
    ("total_issues", .num s.totalIssues),
    ("open_issues", .num s.openIssues),
    ("in_progress_issues", .num s.inProgressIssues),
    ("closed_issues", .num s.closedIssues),
    ("blocked_issues", .num s.blockedIssues),
    ("ready_issues", .num s.readyIssues),
    ("epics_eligible_for_closure", .num s.epicsEligibleForClosure)
  ]

end Beads
