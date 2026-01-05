#!/bin/bash
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
TARGET_DIR="$HOME/cli-apps/scripts"

echo "Building org-warrior..."

# Ensure target directory exists
mkdir -p "$TARGET_DIR"

# Copy the executable
cp "$PROJECT_DIR/src/org-warrior" "$TARGET_DIR/org-warrior"
chmod +x "$TARGET_DIR/org-warrior"

echo "✓ Built and installed org-warrior to $TARGET_DIR/org-warrior"
echo ""
echo "Make sure $TARGET_DIR is in your PATH:"
echo "  export PATH=\"\$HOME/cli-apps/scripts:\$PATH\""
