# mergers

[![workflow status](https://github.com/joske/mergers/actions/workflows/ci.yml/badge.svg)](https://github.com/joske/mergers/actions?query=workflow%3A%22CI%22)
[![security audit](https://github.com/joske/mergers/actions/workflows/audit.yml/badge.svg)](https://github.com/joske/mergers/actions?query=workflow%3A%22Security+Audit%22)

A visual diff and merge tool written in Rust with GTK4, inspired by [Meld](https://meldmerge.org/).

> **WARNING: This software is alpha-quality and under active development. It may eat your files, corrupt your data, or silently discard changes. Always keep backups and use version control. You have been warned.**

## Features

- **Two-way file comparison** with syntax highlighting (GtkSourceView 5)
- **Three-way merge** for resolving conflicts
- **Directory comparison** with recursive scanning and async background diffing
- **Git integration** — run `mergers .` to view uncommitted changes
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

## Installation

### macOS (Homebrew)

```bash
brew install joske/mergers/mergers
```

This downloads a pre-built binary (apple silicon - on intel it will build from source) and pulls in GTK4 and GtkSourceView 5 automatically.

### Linux (Arch)

mergers is in the AUR, use your AUR helper of choice:

```
yay -S mergers
```

### Linux (AppImage)

Download the latest AppImage from [GitHub Releases](https://github.com/joske/mergers/releases/latest):

```bash
chmod +x mergers-x86_64.AppImage
./mergers-x86_64.AppImage
```

### Windows (x86_64 & aarch64)

Download the latest build from [GitHub Releases](https://github.com/joske/mergers/releases/latest):

### From crates.io

Requires GTK4 and GtkSourceView 5 development libraries (see [Building](#building)).

```bash
cargo install mergers
```

### From source

See [Building](#building) below.

## Usage

```
# Compare two files
mergers file1.txt file2.txt

# Compare two directories
mergers dir1/ dir2/

# Three-way merge (left, merged, right)
mergers local.txt merged.txt remote.txt

# View uncommitted git changes
mergers .

# Custom labels
mergers file1.txt file2.txt -L "Original" -L "Modified"
```

## Git Mergetool

To use mergers as your git merge tool:

```bash
git config merge.tool mergers
git config mergetool.mergers.cmd 'mergers "$LOCAL" "$MERGED" "$REMOTE"'
git config mergetool.mergers.trustExitCode true
```

Then resolve conflicts with:

```bash
git mergetool
```

## Comparison with Similar Tools

| Feature                           | mergers               | [Meld](https://meldmerge.org/) | [Diffuse](https://github.com/MightyCreak/diffuse) |
| --------------------------------- | --------------------- | ------------------------------ | ------------------------------------------------- |
| **File Comparison**               | 2-way, 3-way          | 2-way, 3-way                   | N-way (arbitrary)                                 |
| **Directory Comparison**          | Yes                   | Yes                            | No                                                |
| **VCS Integration**               | Git                   | Git, Hg, Bzr, CVS, SVN         | Git, Hg, Bzr, CVS, SVN, Darcs, Monotone, RCS      |
| **Syntax Highlighting**           | Yes (GtkSourceView 5) | Yes (GtkSourceView)            | Yes                                               |
| **Word-level Diffs**              | Yes                   | Yes                            | No                                                |
| **Inline Editing**                | Yes                   | Yes                            | Yes                                               |
| **Manual Line Alignment**         | No                    | No                             | Yes                                               |
| **Undo/Redo**                     | Yes                   | Yes                            | Unlimited undo                                    |
| **Find & Replace**                | Yes                   | Yes                            | Yes                                               |
| **Go to Line**                    | Yes                   | No                             | No                                                |
| **Patch Export**                  | Yes                   | No                             | No                                                |
| **Ignore Whitespace**             | Yes                   | Yes                            | No                                                |
| **Ignore Blank Lines**            | Yes                   | Yes                            | No                                                |
| **Text Wrapping Options**         | Yes                   | Yes                            | No                                                |
| **File Monitoring / Auto-reload** | Yes                   | Yes                            | No                                                |
| **Chunk Map (minimap)**           | Yes                   | Yes                            | No                                                |
| **Conflict Marker Detection**     | Yes                   | No                             | No                                                |
| **Folder Filters**                | Yes                   | Yes                            | N/A                                               |
| **Git Mergetool Support**         | Yes                   | Yes                            | Yes                                               |
| **Custom Pane Labels**            | Yes                   | Yes                            | Yes                                               |
| **Toolkit**                       | GTK4                  | GTK3                           | GTK3                                              |
| **Language**                      | Rust                  | Python                         | Python                                            |
| **License**                       | GPL-2.0               | GPL-2.0                        | GPL-2.0                                           |

## Building

Requires GTK4 and GtkSourceView 5 development libraries.

```
# Debian/Ubuntu
sudo apt install libgtk-4-dev libgtksourceview-5-dev

# Fedora
sudo dnf install gtk4-devel gtksourceview5-devel

# Arch
sudo pacman -S gtk4 gtksourceview5

# macOS
brew install gtk4 gtksourceview5

# Build
cargo build --release
```

## Testing

mergers uses a three-layer testing strategy:

**Layer 1 — Pure unit tests (Rust)**

Core algorithms (Myers diff, chunk navigation, merge logic, gutter hit-testing, chunk-map geometry) are extracted into pure functions in `src/ui/diff_state.rs` and `src/ui/merge_state.rs` and covered by `cargo test`. These run headlessly on every CI push — no display server needed.

```bash
cargo test
```

**Layer 2 — UI integration tests (dogtail / AT-SPI)**

Higher-level tests drive the real GTK4 application via the AT-SPI accessibility bus using [dogtail](https://gitlab.gnome.org/GNOME/dogtail). They exercise file comparison, directory comparison, VCS mode, and keyboard shortcuts end-to-end. An X virtual framebuffer (Xvfb) is used so tests run headlessly in CI.

Test files live in `tests/ui_integration/`. Dependencies are managed in a local venv:

```bash
# First time — sets up the venv and installs dogtail
make test-release

# Subsequent runs
make test-release
```

`make test-release` runs `cargo test` followed by `pytest tests/ui_integration/ -v` under `xvfb-run`.

**Running UI tests manually (with a display)**

```bash
source tests/ui_integration/.venv/bin/activate
pytest tests/ui_integration/ -v
```

**CI**

The `.github/workflows/ui-tests.yml` workflow runs the full suite on every push, installing `at-spi2-core` and `xdotool` from the system package manager before invoking `make test-release`.

## License

GPL-2.0
