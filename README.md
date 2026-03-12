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
- **Ignore whitespace / blank lines** options (with persistent defaults in preferences)
- **Find & Replace** within panes
- **Go to line** (Ctrl+L)
- **Undo / Redo**
- **Chunk navigation** with keyboard shortcuts (with optional wrap-around)
- **Synchronized scrolling** — chunk-aware vertical sync keeps corresponding lines aligned
- **Chunk map** — visual overview strips alongside each pane with viewport indicator
- **Patch viewing** — open `.patch`/`.diff` files to interactively review and apply changes, with conflict markers for failed hunks
- **Patch export** (unified diff format)
- **Refresh** (Ctrl+R / F5) — reload files from disk with unsaved-changes confirmation
- **Open externally** (Ctrl+Shift+O) — open focused file in system default app
- **Save As** (Ctrl+Shift+S) — save focused pane to a new path
- **Save All** (Ctrl+Shift+L) — save all dirty panes at once
- **Blank Comparison** — start with empty panes for quick scratch diffs
- **New Comparison** (Ctrl+N) — open a new comparison tab from any window
- **Tab navigation** — Ctrl+Alt+PageUp/Down and Alt+1-9 to switch tabs
- **Swap panes**
- **Custom pane labels** via `-L` flag
- **Directory copy/delete** operations
- **File watcher** — auto-reloads when files change on disk
- **Configurable folder filters** (exclude patterns like `node_modules`, `.git`, etc.)
- **Insert spaces instead of tabs** option
- **Show whitespace characters** (spaces, tabs, newlines)
- **Keyboard shortcuts reference** — accessible from the preferences dialog
- **Preferences dialog** — font, color scheme, tab width, editor & comparison defaults, filters, with live preview
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

# Apply a patch to a file (auto-detected from .patch/.diff extension)
mergers original.txt changes.patch

# Apply a patch to a directory
mergers project/ changes.patch

# View uncommitted git changes
mergers .

# Custom labels
mergers file1.txt file2.txt -L "Original" -L "Modified"
```

## Git Mergetool

To use mergers as your git merge tool:

```bash
git config --global merge.tool mergers
git config --global mergetool.mergers.cmd 'mergers "$LOCAL" "$MERGED" "$REMOTE"'
git config --global mergetool.mergers.trustExitCode true
```

Then resolve conflicts with:

```bash
git mergetool
```

## Comparison with Similar Tools

| Feature                           | mergers               | [Meld](https://meldmerge.org/) | [Diffuse](https://github.com/MightyCreak/diffuse) | [Kompare](https://apps.kde.org/kompare/) |
| --------------------------------- | --------------------- | ------------------------------ | ------------------------------------------------- | ---------------------------------------- |
| **File Comparison**               | 2-way, 3-way          | 2-way, 3-way                   | N-way (arbitrary)                                 | 2-way                                    |
| **Directory Comparison**          | Yes                   | Yes                            | No                                                | Yes                                      |
| **VCS Integration**               | Git                   | Git, Hg, Bzr, CVS, SVN         | Git, Hg, Bzr, CVS, SVN, Darcs, Monotone, RCS      | No                                       |
| **Syntax Highlighting**           | Yes (GtkSourceView 5) | Yes (GtkSourceView)            | Yes                                               | No                                       |
| **Word-level Diffs**              | Yes                   | Yes                            | No                                                | No                                       |
| **Inline Editing**                | Yes                   | Yes                            | Yes                                               | No                                       |
| **Manual Line Alignment**         | No                    | No                             | Yes                                               | No                                       |
| **Undo/Redo**                     | Yes                   | Yes                            | Unlimited undo                                    | No                                       |
| **Find & Replace**                | Yes                   | Yes                            | Yes                                               | No                                       |
| **Go to Line**                    | Yes                   | No                             | No                                                | No                                       |
| **Patch Export**                  | Yes                   | No                             | No                                                | Yes                                      |
| **Patch Viewing/Applying**        | Yes                   | No                             | No                                                | Yes                                      |
| **Ignore Whitespace**             | Yes                   | Yes                            | No                                                | No                                       |
| **Ignore Blank Lines**            | Yes                   | Yes                            | No                                                | No                                       |
| **Text Wrapping Options**         | Yes                   | Yes                            | No                                                | No                                       |
| **File Monitoring / Auto-reload** | Yes                   | Yes                            | No                                                | No                                       |
| **Synchronized Scrolling**        | Yes                   | Yes                            | No                                                | No                                       |
| **Chunk Map (minimap)**           | Yes                   | Yes                            | No                                                | No                                       |
| **Conflict Marker Detection**     | Yes                   | No                             | No                                                | No                                       |
| **Diff Statistics**               | No                    | No                             | No                                                | Yes                                      |
| **Folder Filters**                | Yes                   | Yes                            | N/A                                               | No                                       |
| **Git Mergetool Support**         | Yes                   | Yes                            | Yes                                               | No                                       |
| **Custom Pane Labels**            | Yes                   | Yes                            | Yes                                               | No                                       |
| **Remote File Support**           | Via GVFS              | Via GVFS                       | No                                                | Via KIO                                  |
| **Toolkit**                       | GTK4                  | GTK3                           | GTK3                                              | Qt (KDE Frameworks)                      |
| **Language**                      | Rust                  | Python                         | Python                                            | C++                                      |
| **License**                       | GPL-2.0               | GPL-2.0                        | GPL-2.0                                           | GPL-2.0                                  |

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

`make test-release` runs `cargo test` followed by `pytest tests/ui_integration/ -v` under `dbus-run-session` + `xvfb-run`.

**Required system packages (Debian/Ubuntu)**

```bash
sudo apt install \
  libgtksourceview-5-dev \
  xvfb \
  fonts-dejavu-core \
  at-spi2-core \
  gir1.2-atspi-2.0 \
  gir1.2-gtk-3.0 \
  dbus-x11 \
  xdotool \
  python3-pip \
  python3-gi
```

**Running UI tests manually (with a display)**

```bash
source tests/ui_integration/.venv/bin/activate
pytest tests/ui_integration/ -v
```

**Layer 3 — Benchmarks (Criterion)**

Performance-critical paths are benchmarked with [Criterion](https://bheisler.github.io/criterion.rs/book/) at large-project scale (50k–500k lines). Benchmarks cover Myers diff, chunk navigation, conflict detection, VCS parsing, and merge state operations. Property tests (proptest) verify optimized algorithms match naive reference implementations.

```bash
cargo bench
```

Benchmarks run automatically on pull requests via `.github/workflows/bench.yml`.

**CI**

The `.github/workflows/ui-tests.yml` workflow runs the full suite on every push, installing `at-spi2-core` and `xdotool` from the system package manager before invoking `make test-release`.

## License

GPL-2.0
