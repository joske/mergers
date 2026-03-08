# Pane Switching, Pull Change, Delete Change, Fullscreen

## Date: 2026-03-08

## Features

### 1. Pane Switching (Alt+PageUp / Alt+PageDown)

Cycle keyboard focus between source view panes with wrap-around.

- **Alt+PageDown** → next pane (left→right in 2-way, left→middle→right in 3-way, wraps)
- **Alt+PageUp** → previous pane (reverse direction, wraps)
- Implementation: use `active_view` to find current pane index, compute next via modulo, call `grab_focus()` on target text view
- Reference: Meld `action_next_pane` / `action_prev_pane` in filediff.py

### 2. Pull Change (Alt+Shift+Left / Alt+Shift+Right)

Copy chunk content FROM an adjacent pane INTO the focused pane. Reverse of existing push (Alt+Left/Right).

- **Alt+Shift+Right** → pull from right pane into focused pane
- **Alt+Shift+Left** → pull from left pane into focused pane
- Reuses existing `replace_chunk` / `apply_chunk` logic with swapped source and destination
- Reference: Meld `action_pull_change_left/right` — calls `get_action_panes(direction, reverse=True)` then `replace_chunk(src, dst, chunk)`

### 3. Delete Change (Alt+Delete)

Delete the current chunk's text entirely from the focused pane.

- Removes all lines in the current chunk range from the focused buffer
- Does NOT replace with content from other side — just deletes
- Edge case: if chunk is at end of buffer, also remove preceding newline to avoid trailing blank line
- Reference: Meld `action_delete_change` → `delete_chunk(pane, chunk)` which calls `b0.delete(start_iter, end_iter)`

### 4. Fullscreen (F11)

Toggle fullscreen mode on the application window.

- Window-level stateful action (not pane-specific)
- Toggle between `window.fullscreen()` and `window.unfullscreen()`
- Check current window state to avoid redundant calls
- Applies to all window types (file diff, dir, merge, VCS)
- Reference: Meld `action_fullscreen_change` in meldwindow.py using `Gio.SimpleAction.new_stateful`

## Files to Modify

- `src/ui/common/helpers.rs` — extend `KeyBindings` and `map_key_to_action()` for new keybindings
- `src/ui/diff_view.rs` — add pane switch, pull, delete actions for 2-way diff
- `src/ui/merge_view.rs` — add pane switch, pull, delete actions for 3-way merge
- `src/ui/file_window.rs` — add F11 fullscreen toggle (window-level)
- `src/ui/dir_window.rs` — add F11 fullscreen toggle
- `src/ui/vcs_window.rs` — add F11 fullscreen toggle
