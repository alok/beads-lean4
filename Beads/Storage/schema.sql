-- Beads SQLite Schema
-- Stores issues, dependencies, labels, and events

-- Issues table
CREATE TABLE IF NOT EXISTS issues (
    id TEXT PRIMARY KEY,
    title TEXT NOT NULL,
    description TEXT DEFAULT '',
    design TEXT DEFAULT '',
    acceptance_criteria TEXT DEFAULT '',
    notes TEXT DEFAULT '',
    status TEXT NOT NULL DEFAULT 'open' CHECK (status IN ('open', 'in_progress', 'blocked', 'closed')),
    priority INTEGER NOT NULL DEFAULT 2 CHECK (priority >= 0 AND priority <= 4),
    issue_type TEXT NOT NULL DEFAULT 'task' CHECK (issue_type IN ('bug', 'feature', 'task', 'epic', 'chore')),
    assignee TEXT,
    estimated_minutes INTEGER,
    created_at INTEGER NOT NULL,
    updated_at INTEGER NOT NULL,
    closed_at INTEGER,
    close_reason TEXT DEFAULT '',
    external_ref TEXT
);

-- Dependencies table
CREATE TABLE IF NOT EXISTS dependencies (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    issue_id TEXT NOT NULL REFERENCES issues(id) ON DELETE CASCADE,
    depends_on_id TEXT NOT NULL REFERENCES issues(id) ON DELETE CASCADE,
    dep_type TEXT NOT NULL DEFAULT 'blocks' CHECK (dep_type IN ('blocks', 'related', 'parent-child', 'discovered-from')),
    created_at INTEGER NOT NULL,
    created_by TEXT NOT NULL,
    UNIQUE(issue_id, depends_on_id)
);

-- Labels table (many-to-many)
CREATE TABLE IF NOT EXISTS labels (
    issue_id TEXT NOT NULL REFERENCES issues(id) ON DELETE CASCADE,
    label TEXT NOT NULL,
    PRIMARY KEY (issue_id, label)
);

-- Events table (audit trail)
CREATE TABLE IF NOT EXISTS events (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    issue_id TEXT NOT NULL REFERENCES issues(id) ON DELETE CASCADE,
    event_type TEXT NOT NULL,
    actor TEXT NOT NULL,
    old_value TEXT,
    new_value TEXT,
    comment TEXT,
    created_at INTEGER NOT NULL
);

-- Indexes for common queries
CREATE INDEX IF NOT EXISTS idx_issues_status ON issues(status);
CREATE INDEX IF NOT EXISTS idx_issues_priority ON issues(priority);
CREATE INDEX IF NOT EXISTS idx_issues_assignee ON issues(assignee);
CREATE INDEX IF NOT EXISTS idx_issues_created_at ON issues(created_at);
CREATE INDEX IF NOT EXISTS idx_dependencies_issue_id ON dependencies(issue_id);
CREATE INDEX IF NOT EXISTS idx_dependencies_depends_on_id ON dependencies(depends_on_id);
CREATE INDEX IF NOT EXISTS idx_labels_label ON labels(label);
CREATE INDEX IF NOT EXISTS idx_events_issue_id ON events(issue_id);

-- View for blocked issues (issues with unclosed blocking dependencies)
CREATE VIEW IF NOT EXISTS blocked_issues AS
SELECT DISTINCT i.*
FROM issues i
JOIN dependencies d ON i.id = d.issue_id
JOIN issues blocker ON d.depends_on_id = blocker.id
WHERE d.dep_type IN ('blocks', 'parent-child')
  AND blocker.status != 'closed'
  AND i.status IN ('open', 'in_progress');

-- View for ready issues (open, not blocked)
CREATE VIEW IF NOT EXISTS ready_issues AS
SELECT i.*
FROM issues i
WHERE i.status IN ('open', 'in_progress')
  AND i.id NOT IN (SELECT id FROM blocked_issues);
