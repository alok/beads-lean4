# BeadsBrowser Verso Genre Design

A custom Verso genre for rendering beads issue tracker data with embedded dependency graphs.

## Overview

BeadsBrowser extends Verso with issue-tracker-aware elements:
- Inline issue references with live status
- Block-level dependency graphs (rendered as SVG)
- Dashboard views for project status

## Genre Definition

```lean
/-- Issue reference that renders with status badge -/
structure IssueRef where
  id : IssueId
  showStatus : Bool := true

/-- Dependency graph block -/
structure DepGraph where
  rootId : IssueId
  maxDepth : Nat := 5
  format : GraphFormat := .mermaid

inductive GraphFormat where
  | mermaid
  | dot
  | svg

/-- Dashboard showing project statistics -/
structure Dashboard where
  showReady : Bool := true
  showBlocked : Bool := true
  showStats : Bool := true

/-- The BeadsBrowser genre -/
def BeadsBrowser : Genre where
  Inline := IssueRef
  Block := DepGraph ⊕ Dashboard
  PartMetadata := Unit  -- No special part metadata
  TraverseContext := BeadsBrowser.Context
  TraverseState := BeadsBrowser.State
```

## Traverse Context & State

```lean
structure BeadsBrowser.Context where
  /-- Path to .beads directory -/
  beadsDir : System.FilePath
  /-- Storage operations -/
  storage : StorageOps

structure BeadsBrowser.State where
  /-- Cached issues -/
  issues : Std.HashMap IssueId Issue := {}
  /-- Generated graph SVGs -/
  graphs : Std.HashMap IssueId String := {}
  /-- Statistics -/
  stats : Option Statistics := none
```

## User API

### Inline Elements

```lean
/-- Reference an issue: {issue bd-abc123} -/
def issue (id : String) : Inline BeadsBrowser :=
  .other { id := ⟨id⟩ } #[]

/-- Reference with custom text: {issueLink bd-abc123}[Fix the bug] -/
def issueLink (id : String) (content : Array (Inline BeadsBrowser)) : Inline BeadsBrowser :=
  .other { id := ⟨id⟩, showStatus := true } content
```

### Block Elements

```lean
/-- Show dependency graph -/
syntax "{depGraph " str "}" : block

/-- Show project dashboard -/
syntax "{dashboard}" : block
```

## HTML Rendering

### Issue Reference
```html
<span class="beads-issue beads-status-open">
  <a href="#issue-bd-abc123">bd-abc123</a>
  <span class="beads-badge">open</span>
</span>
```

### Dependency Graph
```html
<figure class="beads-dep-graph">
  <svg><!-- mermaid-rendered SVG --></svg>
  <figcaption>Dependency graph for bd-abc123</figcaption>
</figure>
```

### Dashboard
```html
<div class="beads-dashboard">
  <div class="beads-stats">
    <span class="stat">Open: 5</span>
    <span class="stat">In Progress: 2</span>
    <span class="stat">Blocked: 1</span>
  </div>
  <h3>Ready Work</h3>
  <ul class="beads-ready">
    <li>bd-abc: Fix login bug</li>
  </ul>
</div>
```

## CSS Styles

```css
.beads-issue { font-family: monospace; }
.beads-status-open { color: #28a745; }
.beads-status-closed { color: #6c757d; }
.beads-status-blocked { color: #dc3545; }
.beads-badge {
  font-size: 0.75em;
  padding: 0.1em 0.4em;
  border-radius: 3px;
}
.beads-dep-graph svg { max-width: 100%; }
```

## Implementation Notes

1. **Mermaid Rendering**: Use mermaid-cli at build time to convert mermaid to SVG
2. **Issue Caching**: Load all issues once during traversal, cache in state
3. **Live Updates**: For web output, could add JavaScript for live updates
4. **Image Storage**: Verso's image support stores as data URLs or external files

## Example Document

```lean
import BeadsBrowser

#doc (BeadsBrowser) "Project Status" =>

# Current Sprint

{dashboard}

# Key Issues

The main blocker is {issue bd-abc123}, which blocks:

{depGraph "bd-abc123"}

Once that's resolved, we can work on {issue bd-def456}.
```

## Dependencies

- Verso (when toolchain stabilizes)
- mermaid-cli for graph rendering
- Beads storage for issue data

## Future Extensions

1. **Kanban View**: Block element showing issues in columns by status
2. **Timeline View**: Gantt-chart style view of issue lifecycle
3. **Burndown Charts**: Track velocity over time
4. **Interactive Graphs**: Click to expand/collapse nodes
