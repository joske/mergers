#!/usr/bin/env bash
set -euo pipefail

appdir=$(realpath "${1:?Usage: lint-appdir.sh APPDIR}")
lint_dir=$(mktemp -d)
trap 'rm -rf "$lint_dir"' EXIT

# Use the same linter as the AppImage catalog, pinned for reproducible checks.
lint_revision=19e30b276ffedf4d3b4b56bc6320f463625a74f8
lint_url="https://raw.githubusercontent.com/AppImage/AppImages/$lint_revision"
curl -fsSL --retry 3 "$lint_url/appdir-lint.sh" -o "$lint_dir/appdir-lint.sh"
curl -fsSL --retry 3 "$lint_url/excludelist" -o "$lint_dir/excludelist"

# Fatal packaging/metadata errors fail CI. Upstream recommendations remain warnings.
bash "$lint_dir/appdir-lint.sh" "$appdir"
