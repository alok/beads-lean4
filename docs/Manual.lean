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

# Quick Start

## Installation

```
git clone https://github.com/alok/beads-lean4
cd beads-lean4
lake build
```

## Create Your First Issue

```
beads create "Fix login bug" "Users cannot login"
```

# Core Concepts

## Issues

Every issue has:

 * *ID*: Collision-resistant hash-based ID (e.g., `bd-a1b2c3`)
 * *Status*: `open`, `in_progress`, or `closed`
 * *Priority*: P0 (critical) to P4 (low)
 * *Type*: `task`, `bug`, `feature`, or `epic`

## Dependencies

Issues can depend on each other with four relationship types:

 * *blocks*: Issue A blocks Issue B (affects ready work)
 * *related*: Informational link
 * *parent-child*: Hierarchical decomposition
 * *discovered-from*: Issue B was found while working on A

# Command Reference

## Creating Issues

```
beads create "Title" "Description"
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
beads ready
beads ready --assignee alice
beads ready --unassigned
beads blocked
beads stats
```

# JSON Output for Agents

All commands support `--json` for machine-readable output:

```
beads --json list
beads --json stats
beads --json create "New task"
```

# Storage Format

Issues are stored in `.beads/issues.jsonl`:

```
{"id":"bd-a1b2c3","title":"Fix bug","status":"open","priority":2,...}
{"id":"bd-d4e5f6","title":"Add feature","status":"closed",...}
```

# Architecture

## Core Types

 * `Types.lean`: Status, Priority, IssueType, DependencyType
 * `Issue.lean`: Issue, Dependency, BlockedIssue structures
 * `Id.lean`: Hash-based ID generation with collision resistance
 * `Json.lean`: Serialization for all types

## Storage

 * `Storage.lean`: Abstract StorageOps interface
 * `Jsonl.lean`: JSONL file-backed implementation with BFS cycle detection

## CLI

Full command-line interface with argument parsing and JSON output support.

# Examples

## Agent Workflow

```
READY=$(beads --json ready --unassigned --limit 1)
ISSUE_ID=$(echo $READY | jq -r '.[0].id')
beads update $ISSUE_ID --status in_progress --assignee agent
beads close $ISSUE_ID "Implemented in PR #123"
```

## Project Planning

```
beads create "User Authentication"
beads create "Login page"
beads dep add bd-login bd-auth --type parent-child
beads create "OAuth integration"
beads dep add bd-oauth bd-login
beads dep tree bd-auth
```

# License

MIT License - See repository for details.
