/-
Beads Documentation - Built with Verso
A git-backed distributed issue tracker in Lean 4
-/
import VersoBlog

open Verso Genre Blog

set_option pp.rawOnError true

#doc (Page) "Beads: Git-Backed Issue Tracker" =>

Beads is a distributed issue tracker designed to be:

 * *Git-native*: Issues stored as JSONL files, perfect for version control
 * *Agent-friendly*: Every command supports `--json` output
 * *Type-safe*: Written in Lean 4 with compile-time guarantees
 * *Dependency-aware*: Track blocking relationships with cycle detection

# Core Types

The core types in Beads provide compile-time safety through Lean 4's type system.

## Status

Issue status is an inductive type with four values:

```lean
inductive Status where
  | open       -- Available for work
  | inProgress -- Currently being worked on
  | blocked    -- Waiting on dependencies
  | closed     -- Completed
```

The `isOpenForWork` method returns `true` for `open` and `inProgress` statuses.

## Issue Type

```lean
inductive IssueType where
  | bug      -- Something broken
  | feature  -- New capability
  | task     -- General work item
  | epic     -- Container for related issues
  | chore    -- Maintenance work
```

## Dependency Type

Four relationship types between issues:

```lean
inductive DependencyType where
  | blocks         -- A blocks B: B cannot start until A closes
  | related        -- Informational link only
  | parentChild    -- B is a subtask of A
  | discoveredFrom -- B was found while working on A
```

The `affectsBlocking` property is `true` for `blocks` and `parentChild` - these types
can prevent an issue from being "ready" for work.

## Priority

Priority uses `Fin 5`, guaranteeing values 0-4 at compile time:

 * *P0*: Critical - drop everything
 * *P1*: High - address soon
 * *P2*: Normal (default)
 * *P3*: Low - when time permits
 * *P4*: Backlog

# Issue Structure

```lean
structure Issue where
  id : IssueId              -- Hash-based, collision-resistant
  title : String
  description : String
  status : Status
  priority : Priority       -- Fin 5, compile-time bounded
  issueType : IssueType
  assignee : Option String
  labels : List String
  createdAt : Nat           -- Unix timestamp
  updatedAt : Nat
  closedAt : Option Nat     -- Set when status = closed
```

Issue IDs use the format `bd-XXXXXX` where X is a hex digit, derived from
a SHA256 hash of title, description, and timestamp.

# Dependency Structure

```lean
structure Dependency where
  issueId : IssueId       -- The dependent issue
  dependsOnId : IssueId   -- The dependency
  depType : DependencyType
  createdAt : Nat
  createdBy : String
```

# Graph Algorithms

## Cycle Detection

Beads uses BFS to detect cycles before adding dependencies:

```lean
def wouldCreateCycle (g : Graph) (from_ to : IssueId) : Bool :=
  if from_ == to then true       -- Self-loop
  else hasPath g to from_        -- Path from target back to source
```

## Blocked Set Computation

An issue is blocked if:
1. It has a `blocks` or `parentChild` dependency to an open issue
2. Its parent is blocked (one level propagation)

```lean
def computeBlockedSet (issues : List Issue) (deps : List Dependency) : HashSet IssueId
```

## Ready Issues

Ready issues are open and not blocked:

```lean
def getReadyIssues (issues : List Issue) (deps : List Dependency) : List Issue :=
  issues.filter fun issue =>
    issue.status.isOpenForWork && !blocked.contains issue.id
```

# Command Reference

## Creating Issues

```
beads create "Title" "Description"
beads create "Title" --type bug --priority 0
beads --json create "Implement feature X"
```

## Listing and Filtering

```
beads list
beads list --status open
beads list --status in_progress
beads list --priority 0
beads list --search "bug"
beads list --status open --priority 1 --limit 10
```

## Viewing Issue Details

```
beads show bd-a1b2c3
beads --json show bd-a1b2c3
```

## Updating Issues

```
beads update bd-a1b2c3 --status in_progress
beads update bd-a1b2c3 --priority 0 --assignee "alice"
beads update bd-a1b2c3 --assignee none
```

## Closing Issues

```
beads close bd-a1b2c3 "Fixed in commit abc123"
beads close bd-a1b2c3 --reason "Won't fix"
```

## Labels

```
beads label add bd-a1b2c3 bug
beads label add bd-a1b2c3 urgent
beads label list bd-a1b2c3
beads label remove bd-a1b2c3 urgent
```

## Dependencies

```
beads dep add bd-blocked bd-blocker
beads dep add bd-child bd-parent --type parent-child
beads dep tree bd-a1b2c3
```

## Workflow Commands

```
beads ready                    -- List unblocked, open issues
beads ready --assignee alice   -- Filter by assignee
beads ready --unassigned       -- Only unassigned issues
beads blocked                  -- List blocked issues with reasons
beads stats                    -- Summary statistics
```

# JSON Output for Agents

All commands support `--json` for machine-readable output:

```
beads --json list
beads --json stats
beads --json create "New task"
```

Example agent workflow:

```bash
READY=$(beads --json ready --unassigned --limit 1)
ISSUE_ID=$(echo $READY | jq -r '.[0].id')
beads update $ISSUE_ID --status in_progress --assignee agent
# ... do work ...
beads close $ISSUE_ID "Implemented in PR #123"
```

# Storage Format

Issues are stored in `.beads/issues.jsonl`:

```json
{"id":"bd-a1b2c3","title":"Fix bug","status":"open","priority":2,...}
{"id":"bd-d4e5f6","title":"Add feature","status":"closed",...}
```

The JSONL format is ideal for git:
 * Each issue is one line → clean diffs
 * No merge conflicts on different issues
 * Human-readable for debugging

# Architecture

## Module Structure

```
Beads/
├── Core/
│   ├── Types.lean      -- Status, IssueType, DependencyType, Priority
│   ├── Issue.lean      -- Issue, Dependency, IssueId structures
│   ├── Id.lean         -- Hash-based ID generation
│   ├── Json.lean       -- JSON serialization
│   ├── Graph.lean      -- Cycle detection, blocking computation
│   └── Validated.lean  -- Type-safe validated issues
└── Storage/
    ├── Storage.lean    -- StorageOps typeclass
    ├── Jsonl.lean      -- JSONL file backend
    └── Sqlite/         -- SQLite backend (optional)
```

## Property Testing

Beads includes 70+ property-based tests using Plausible:

 * ID generation collision resistance
 * JSON roundtrip for all types
 * Cycle detection correctness
 * Blocking computation semantics
 * Type invariants (Priority bounds, closed timestamps)

# License

MIT License - See repository for details.
