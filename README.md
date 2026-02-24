# meld-rs

[![workflow status](https://github.com/joske/meld-rs/actions/workflows/ci.yml/badge.svg)](https://github.com/joske/meld-rs/actions?query=workflow%3A%22CI%22)
[![security audit](https://github.com/joske/meld-rs/actions/workflows/audit.yml/badge.svg)](https://github.com/joske/meld-rs/actions?query=workflow%3A%22Security+Audit%22)

A visual diff and merge tool written in Rust with GTK4, inspired by [Meld](https://meldmerge.org/).

> **WARNING: This software is alpha-quality and under active development. It may eat your files, corrupt your data, or silently discard changes. Always keep backups and use version control. You have been warned.**

## Features

- **Two-way file comparison** with syntax highlighting (GtkSourceView 5)
- **Three-way merge** for resolving conflicts
- **Directory comparison** with recursive scanning and async background diffing
- **Git integration** — run `meld-rs .` to view uncommitted changes
- Inline **word-level diff** highlighting
- **Ignore whitespace / blank lines** options
- **Find & Replace** within panes
- **Go to line** (Ctrl+L)
- **Undo / Redo**
- **Chunk navigation** with keyboard shortcuts
- **Chunk map** — visual overview strips alongside each pane
- **Patch export** (unified diff format)
- **Swap panes**
- **Custom pane labels** via `-L` flag
- **Directory copy/delete** operations
- **File watcher** — auto-reloads when files change on disk
- **Configurable folder filters** (exclude patterns like `node_modules`, `.git`, etc.)
- **Preferences dialog** — font, color scheme, tab width, filters, with live preview
- **Binary file detection** — prevents loading binary files into text buffers

## Usage

```
# Compare two files
meld-rs file1.txt file2.txt

# Compare two directories
meld-rs dir1/ dir2/

# Three-way merge
meld-rs left.txt middle.txt right.txt
meld-rs left.txt middle.txt right.txt -o output.txt

# View uncommitted git changes
meld-rs .

# Custom labels
meld-rs file1.txt file2.txt -L "Original" -L "Modified"
```

## Building

Requires GTK4 and GtkSourceView 5 development libraries.

```
# Debian/Ubuntu
sudo apt install libgtk-4-dev libgtksourceview-5-dev

# Fedora
sudo dnf install gtk4-devel gtksourceview5-devel

# Arch
sudo pacman -S gtk4 gtksourceview5

# Build
cargo build --release
```

## License

MIT
