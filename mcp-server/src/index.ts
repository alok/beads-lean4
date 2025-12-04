#!/usr/bin/env node
/**
 * Beads MCP Server
 *
 * Exposes the beads issue tracker as MCP tools for Claude Code integration.
 * Wraps the beads CLI with JSON output for structured responses.
 */

import { Server } from "@modelcontextprotocol/sdk/server/index.js";
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";
import {
  CallToolRequestSchema,
  ListToolsRequestSchema,
  Tool,
} from "@modelcontextprotocol/sdk/types.js";
import { spawn } from "child_process";
import { z } from "zod";

// Path to beads binary - configurable via env
const BEADS_BIN = process.env.BEADS_BIN || "beads";
// Working directory for beads - defaults to cwd but can be overridden
const BEADS_CWD = process.env.BEADS_CWD || process.cwd();

// Helper to run beads CLI commands
async function runBeads(args: string[]): Promise<{ success: boolean; output: string; parsed?: unknown }> {
  return new Promise((resolve) => {
    const proc = spawn(BEADS_BIN, ["--json", ...args], {
      cwd: BEADS_CWD,
    });

    let stdout = "";
    let stderr = "";

    proc.stdout.on("data", (data) => {
      stdout += data.toString();
    });

    proc.stderr.on("data", (data) => {
      stderr += data.toString();
    });

    proc.on("close", (code) => {
      const output = stdout || stderr;
      let parsed: unknown;

      try {
        // Try to parse JSON from output
        const jsonMatch = output.match(/\{[\s\S]*\}|\[[\s\S]*\]/);
        if (jsonMatch) {
          parsed = JSON.parse(jsonMatch[0]);
        }
      } catch {
        // Not JSON, that's fine
      }

      resolve({
        success: code === 0,
        output: output.trim(),
        parsed,
      });
    });

    proc.on("error", (err) => {
      resolve({
        success: false,
        output: `Failed to run beads: ${err.message}`,
      });
    });
  });
}

// Tool definitions
const tools: Tool[] = [
  {
    name: "beads_create",
    description: "Create a new issue in the beads tracker. Returns the created issue with its generated ID.",
    inputSchema: {
      type: "object" as const,
      properties: {
        title: { type: "string", description: "Issue title (required)" },
        description: { type: "string", description: "Issue description" },
        type: {
          type: "string",
          enum: ["task", "bug", "feature", "epic"],
          description: "Issue type (default: task)"
        },
        priority: {
          type: "number",
          minimum: 0,
          maximum: 4,
          description: "Priority 0-4 (0=highest, default: 2)"
        },
      },
      required: ["title"],
    },
  },
  {
    name: "beads_list",
    description: "List issues with optional filters. Returns array of issues.",
    inputSchema: {
      type: "object" as const,
      properties: {
        status: {
          type: "string",
          enum: ["open", "in_progress", "closed"],
          description: "Filter by status"
        },
        type: {
          type: "string",
          enum: ["task", "bug", "feature", "epic"],
          description: "Filter by type"
        },
        assignee: { type: "string", description: "Filter by assignee" },
        search: { type: "string", description: "Search in title" },
        limit: { type: "number", description: "Max results (default: 100)" },
      },
    },
  },
  {
    name: "beads_show",
    description: "Show details of a specific issue including dependencies.",
    inputSchema: {
      type: "object" as const,
      properties: {
        id: { type: "string", description: "Issue ID (e.g., bd-a3f8e9)" },
      },
      required: ["id"],
    },
  },
  {
    name: "beads_update",
    description: "Update an existing issue's fields.",
    inputSchema: {
      type: "object" as const,
      properties: {
        id: { type: "string", description: "Issue ID to update" },
        title: { type: "string", description: "New title" },
        description: { type: "string", description: "New description" },
        status: {
          type: "string",
          enum: ["open", "in_progress", "closed"],
          description: "New status"
        },
        priority: { type: "number", minimum: 0, maximum: 4 },
        assignee: { type: "string", description: "Assignee (or 'none' to unassign)" },
      },
      required: ["id"],
    },
  },
  {
    name: "beads_close",
    description: "Close an issue with optional reason.",
    inputSchema: {
      type: "object" as const,
      properties: {
        id: { type: "string", description: "Issue ID to close" },
        reason: { type: "string", description: "Reason for closing" },
      },
      required: ["id"],
    },
  },
  {
    name: "beads_ready",
    description: "List ready work - open issues that are not blocked by other open issues.",
    inputSchema: {
      type: "object" as const,
      properties: {
        assignee: { type: "string", description: "Filter by assignee" },
        unassigned: { type: "boolean", description: "Show only unassigned issues" },
        limit: { type: "number", description: "Max results" },
      },
    },
  },
  {
    name: "beads_blocked",
    description: "List blocked issues - issues waiting on other open issues.",
    inputSchema: {
      type: "object" as const,
      properties: {},
    },
  },
  {
    name: "beads_dep_add",
    description: "Add a dependency between issues. Creates a blocking relationship.",
    inputSchema: {
      type: "object" as const,
      properties: {
        from: { type: "string", description: "Issue that is blocked" },
        to: { type: "string", description: "Issue that blocks it" },
        type: {
          type: "string",
          enum: ["blocks", "related", "parent-child", "discovered-from"],
          description: "Dependency type (default: blocks)"
        },
      },
      required: ["from", "to"],
    },
  },
  {
    name: "beads_stats",
    description: "Get issue statistics - counts by status, type, priority.",
    inputSchema: {
      type: "object" as const,
      properties: {},
    },
  },
  {
    name: "beads_sync",
    description: "Sync issues with git remote. Exports, commits, pulls, and pushes.",
    inputSchema: {
      type: "object" as const,
      properties: {
        dryRun: { type: "boolean", description: "Preview without making changes" },
        noPull: { type: "boolean", description: "Skip pulling from remote" },
        noPush: { type: "boolean", description: "Skip pushing to remote" },
      },
    },
  },
  {
    name: "beads_search",
    description: "Search issues by title text.",
    inputSchema: {
      type: "object" as const,
      properties: {
        query: { type: "string", description: "Search query" },
        status: { type: "string", enum: ["open", "in_progress", "closed"] },
        limit: { type: "number" },
      },
      required: ["query"],
    },
  },
  {
    name: "beads_assign",
    description: "Assign an issue to someone (or unassign).",
    inputSchema: {
      type: "object" as const,
      properties: {
        id: { type: "string", description: "Issue ID" },
        assignee: { type: "string", description: "Assignee name (omit to unassign)" },
      },
      required: ["id"],
    },
  },
];

// Tool handlers
async function handleTool(name: string, args: Record<string, unknown>): Promise<string> {
  let cliArgs: string[] = [];

  switch (name) {
    case "beads_create": {
      cliArgs = ["create", args.title as string];
      if (args.description) cliArgs.push(args.description as string);
      if (args.type) cliArgs.push("--type", args.type as string);
      if (args.priority !== undefined) cliArgs.push("--priority", String(args.priority));
      break;
    }

    case "beads_list": {
      cliArgs = ["list"];
      if (args.status) cliArgs.push("--status", args.status as string);
      if (args.type) cliArgs.push("--type", args.type as string);
      if (args.assignee) cliArgs.push("--assignee", args.assignee as string);
      if (args.search) cliArgs.push("--search", args.search as string);
      if (args.limit) cliArgs.push("--limit", String(args.limit));
      break;
    }

    case "beads_show": {
      cliArgs = ["show", args.id as string];
      break;
    }

    case "beads_update": {
      cliArgs = ["update", args.id as string];
      if (args.title) cliArgs.push("--title", args.title as string);
      if (args.description) cliArgs.push("--description", args.description as string);
      if (args.status) cliArgs.push("--status", args.status as string);
      if (args.priority !== undefined) cliArgs.push("--priority", String(args.priority));
      if (args.assignee) cliArgs.push("--assignee", args.assignee as string);
      break;
    }

    case "beads_close": {
      cliArgs = ["close", args.id as string];
      if (args.reason) cliArgs.push("--reason", args.reason as string);
      break;
    }

    case "beads_ready": {
      cliArgs = ["ready"];
      if (args.assignee) cliArgs.push("--assignee", args.assignee as string);
      if (args.unassigned) cliArgs.push("--unassigned");
      if (args.limit) cliArgs.push("--limit", String(args.limit));
      break;
    }

    case "beads_blocked": {
      cliArgs = ["blocked"];
      break;
    }

    case "beads_dep_add": {
      cliArgs = ["dep", "add", args.from as string, args.to as string];
      if (args.type) cliArgs.push("--type", args.type as string);
      break;
    }

    case "beads_stats": {
      cliArgs = ["stats"];
      break;
    }

    case "beads_sync": {
      cliArgs = ["sync"];
      if (args.dryRun) cliArgs.push("--dry-run");
      if (args.noPull) cliArgs.push("--no-pull");
      if (args.noPush) cliArgs.push("--no-push");
      break;
    }

    case "beads_search": {
      cliArgs = ["search", args.query as string];
      if (args.status) cliArgs.push("--status", args.status as string);
      if (args.limit) cliArgs.push("--limit", String(args.limit));
      break;
    }

    case "beads_assign": {
      cliArgs = ["assign", args.id as string];
      if (args.assignee) cliArgs.push(args.assignee as string);
      break;
    }

    default:
      return JSON.stringify({ error: `Unknown tool: ${name}` });
  }

  const result = await runBeads(cliArgs);

  if (result.parsed) {
    return JSON.stringify(result.parsed, null, 2);
  }

  return result.output;
}

// Main server setup
const server = new Server(
  {
    name: "beads-mcp",
    version: "0.1.0",
  },
  {
    capabilities: {
      tools: {},
    },
  }
);

// Register tool list handler
server.setRequestHandler(ListToolsRequestSchema, async () => {
  return { tools };
});

// Register tool call handler
server.setRequestHandler(CallToolRequestSchema, async (request) => {
  const { name, arguments: args } = request.params;

  try {
    const result = await handleTool(name, args as Record<string, unknown>);
    return {
      content: [{ type: "text", text: result }],
    };
  } catch (error) {
    const message = error instanceof Error ? error.message : String(error);
    return {
      content: [{ type: "text", text: `Error: ${message}` }],
      isError: true,
    };
  }
});

// Start server
async function main() {
  const transport = new StdioServerTransport();
  await server.connect(transport);
  console.error("Beads MCP server started");
}

main().catch((error) => {
  console.error("Fatal error:", error);
  process.exit(1);
});
