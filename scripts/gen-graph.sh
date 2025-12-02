#!/bin/bash
# Generate SVG graph from beads dependency tree
# Requires: npm install -g @mermaid-js/mermaid-cli
#
# Usage: ./scripts/gen-graph.sh <issue-id> [output.svg]
#
# Example:
#   ./scripts/gen-graph.sh bd-abc123 graph.svg

set -e

BEADS="${BEADS:-$(dirname "$0")/../.lake/build/bin/beads}"
ISSUE_ID="${1:?Usage: gen-graph.sh <issue-id> [output.svg]}"
OUTPUT="${2:-${ISSUE_ID}.svg}"

# Check for mermaid-cli
if ! command -v mmdc &> /dev/null; then
    echo "Error: mermaid-cli (mmdc) not found"
    echo "Install with: npm install -g @mermaid-js/mermaid-cli"
    exit 1
fi

# Generate mermaid and convert to SVG
TMPFILE=$(mktemp /tmp/beads-mermaid.XXXXXX.mmd)
trap "rm -f $TMPFILE" EXIT

echo "Generating mermaid diagram for $ISSUE_ID..."
$BEADS dep tree "$ISSUE_ID" --mermaid > "$TMPFILE"

echo "Converting to SVG..."
mmdc -i "$TMPFILE" -o "$OUTPUT" -b transparent

echo "Generated: $OUTPUT"

# Also show the mermaid source
echo ""
echo "Mermaid source:"
cat "$TMPFILE"
