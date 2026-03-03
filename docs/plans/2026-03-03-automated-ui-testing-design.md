# Automated UI Testing Design

## Goal

Minimize manual UI testing by automating ~74% of TESTING.md checklist items across three layers: pure Rust unit tests, GTK widget state tests, and a Python/dogtail pre-release integration suite.

## Architecture: Three-Layer Testing Strategy

### Layer 1: Pure Rust Unit Tests (every CI run, no display)

Extract pure logic from GTK callbacks into standalone testable functions. Add CLI integration tests and property-based tests.

**State model extraction** — new files `diff_state.rs` and `merge_state.rs`:

| New pure function | Extracted from | Source location |
|---|---|---|
| `find_next_chunk(chunks, cursor_line, direction, side)` | `navigate_chunk` | common.rs:1567 |
| `format_chunk_label(total, current)` | `update_chunk_label` | common.rs:1645 |
| `find_conflict_markers_in_text(text)` | `find_conflict_markers` | merge_view.rs:181 |
| `hit_test_gutter_arrows(x, y, width, ...)` | `handle_gutter_click` | common.rs:1358 |
| `compute_chunk_map_rects(chunks, height, total_lines)` | `draw_chunk_map` | common.rs:1475 |
| `plan_copy_chunk(chunks, idx, direction, left_text, right_text)` | `copy_chunk` | common.rs:1265 |
| `find_all_matches(text, needle)` | `highlight_search_matches` | common.rs:1424 |
| `swap_state(left, right)` involution | swap callbacks | various |

**CLI integration tests** (`tests/cli_integration.rs`):
- Test all 15 CLI/Launch Mode items via `std::process::Command`
- Assert exit codes and stderr output for each argument combination

**Extended property-based tests**:
- Unified diff format validity roundtrip
- Navigation invariants (next chunk always past cursor)
- `filter_for_diff` + `remap_chunks` roundtrip

### Layer 2: `#[gtk::test]` Widget State Tests (every CI run, needs Xvfb)

Create GTK widgets programmatically and verify their state:

- `build_diff_view` with test data: verify tag placement, button sensitivity, action groups
- `build_merge_view` with 3-way data: verify pane read-only state, conflict overlay
- TextBuffer manipulation: verify save button goes sensitive on edit
- Keybinding registration: verify action names exist in action groups
- Identical/binary file detection: verify info bar creation, button disabled states

**File**: `tests/widget_tests.rs`

### Layer 3: Dogtail + Screenshot Suite (pre-release only)

**Dogtail** (Python AT-SPI) tests in `tests/ui_integration/`:
- File diff workflow: open files, verify chunk count, navigate, copy chunk, save
- Directory comparison: open dir, expand tree, double-click file, verify tab
- 3-way merge: copy from left/right gutter, verify middle buffer
- VCS window: open repo, double-click file, verify diff tab
- Keyboard shortcuts: Ctrl+F, Ctrl+S, Alt+Up/Down, Ctrl+J/K, etc.

**Screenshot regression**:
- Capture views via `WidgetPaintable` -> `Renderer` -> `Texture` -> PNG
- Compare against reference images using `image-compare` crate (SSIM)
- Fuzzy threshold to tolerate font/rendering differences

## Project Structure

```
src/ui/
  diff_state.rs      # NEW - pure diff logic (extracted from common.rs/diff_view.rs)
  merge_state.rs     # NEW - pure merge logic (extracted from merge_view.rs)
  common.rs          # MODIFIED - extracted fns become thin wrappers
  diff_view.rs       # MODIFIED - delegates to diff_state
  merge_view.rs      # MODIFIED - delegates to merge_state

tests/
  vcs_integration.rs   # existing
  cli_integration.rs   # NEW - CLI argument tests
  widget_tests.rs      # NEW - #[gtk::test] widget state tests

tests/ui_integration/  # NEW - Python dogtail suite
  conftest.py          # pytest fixtures (launch app, AT-SPI connection)
  test_file_diff.py
  test_dir_compare.py
  test_merge.py
  test_vcs.py
  test_shortcuts.py
  fixtures/            # test data files
    left.txt / right.txt / base.txt
    left_dir/ / right_dir/
    binary_file.bin
    identical_a.txt / identical_b.txt

tests/screenshots/     # NEW - visual regression
  refs/                # reference PNGs
```

## CI Changes

### Existing CI (`.github/workflows/ci.yml`)

Add xvfb and font packages for widget tests:

```yaml
- name: Install packages
  run: sudo apt-get update && sudo apt-get install -y
    build-essential libgtksourceview-5-dev xvfb fonts-dejavu-core

- name: Run tests
  env:
    GDK_BACKEND: x11
    GTK_A11Y: none
  run: xvfb-run -a cargo test --all-targets --locked
```

### New pre-release workflow (`.github/workflows/ui-tests.yml`)

Manual dispatch for pre-release validation:

```yaml
- Install: xvfb, fonts, python3, pip, at-spi2-registryd, dogtail
- Build: cargo build --release
- Run: xvfb-run pytest tests/ui_integration/ -v
- Run: xvfb-run cargo test --test screenshot_tests
- Upload: screenshot diff artifacts on failure
```

### Convenience targets (`Makefile`)

```makefile
test:          cargo test
test-ui:       xvfb-run -a cargo test
test-release:  xvfb-run -a pytest tests/ui_integration/ && xvfb-run -a cargo test --test screenshot_tests
```

## Coverage Map

| Section | Items | Automated | Rate |
|---|---|---|---|
| CLI / Launch Modes | 15 | 15 | 100% |
| Welcome Window | 6 | 3 | 50% |
| File Diff (all subsections) | 45 | 35 | 78% |
| Navigation | 4 | 4 | 100% |
| Directory Comparison | 30 | 18 | 60% |
| 3-Way Merge | 20 | 13 | 65% |
| VCS Window | 15 | 11 | 73% |
| Preferences | 12 | 6 | 50% |
| Global Shortcuts | 20 | 18 | 90% |
| **Total** | **~167** | **~123** | **~74%** |

### Items that remain manual (~26%)

These are genuinely hard to automate in the GTK4 ecosystem:
- **File dialog interactions**: OS-native portal dialogs, can't drive from within process
- **Visual rendering judgment**: Chunk background colors, gutter curve aesthetics, filler line positions, inline highlight colors
- **Timing-dependent behavior**: File watcher debouncing (500ms poll), save suppression (600ms window), background diff responsiveness
- **Cross-platform specifics**: macOS dark mode detection, Windows build behavior
- **Scroll smoothness**: Subjective UX quality that requires human judgment

## Key Dependencies

### Rust (dev-dependencies)
- `proptest` (existing) - property-based testing
- `tempfile` (existing) - temporary files/dirs
- `image-compare` (new) - SSIM screenshot comparison
- `image` (new) - PNG encoding/decoding

### Python (pre-release only)
- `dogtail` >= 1.0 - AT-SPI UI testing framework
- `pytest` - test runner

### System (CI)
- `xvfb` - virtual X11 display
- `fonts-dejavu-core` - consistent font rendering
- `at-spi2-registryd` - accessibility bus (for dogtail)

## Implementation Order

1. **Extract pure logic** into `diff_state.rs` / `merge_state.rs` + unit tests
2. **CLI integration tests** in `tests/cli_integration.rs`
3. **Property-based test extensions** for unified diff, navigation, filter/remap
4. **CI xvfb setup** + `#[gtk::test]` widget state tests
5. **Dogtail test suite** scaffold + fixture files
6. **Screenshot regression** harness + initial reference images
7. **Makefile** + pre-release workflow
