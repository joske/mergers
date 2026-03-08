# Pane Switching, Pull Change, Delete Change, Fullscreen — Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add four Meld-parity features: Alt+PageUp/Down pane switching, Alt+Shift+Left/Right pull change, Alt+Delete delete change, and F11 fullscreen toggle.

**Architecture:** All four features follow the existing action pattern — add key mappings in `map_key_to_action()`, create `gio::SimpleAction` handlers in `build_diff_view()` / `build_merge_view()`, and register accelerators. Fullscreen is window-level in `build_app_window()`.

**Tech Stack:** Rust, GTK4, gio actions, GtkSourceView5

---

### Task 1: Add key mappings in helpers.rs

**Files:**
- Modify: `src/ui/common/helpers.rs:537-613`

**Step 1: Extend `KeyBindings` struct**

Add two new optional action name fields for Alt+Shift+Left/Right:

```rust
pub struct KeyBindings {
    pub alt_left: &'static str,
    pub alt_right: &'static str,
    pub alt_shift_left: &'static str,
    pub alt_shift_right: &'static str,
    pub extra_ctrl_shift: &'static [(&'static str, gtk4::gdk::Key, gtk4::gdk::Key)],
    pub extra_ctrl: &'static [(&'static str, gtk4::gdk::Key, gtk4::gdk::Key)],
}
```

**Step 2: Add new key mappings in `map_key_to_action()`**

In the `if mods.contains(ModifierType::ALT_MASK)` block, split into Alt+Shift vs plain Alt:

```rust
if mods.contains(ModifierType::ALT_MASK) {
    if mods.contains(ModifierType::SHIFT_MASK) {
        return match key {
            k if k == Key::Left => Some(bindings.alt_shift_left),
            k if k == Key::Right => Some(bindings.alt_shift_right),
            _ => None,
        };
    }
    return match key {
        k if k == Key::Up => Some("prev-chunk"),
        k if k == Key::Down => Some("next-chunk"),
        k if k == Key::Left => Some(bindings.alt_left),
        k if k == Key::Right => Some(bindings.alt_right),
        k if k == Key::Page_Up => Some("prev-pane"),
        k if k == Key::Page_Down => Some("next-pane"),
        k if k == Key::Delete || k == Key::KP_Delete => Some("delete-chunk"),
        _ => None,
    };
}
```

Also add F11 at the end of the function, before the final `None`:

```rust
if key == Key::F11 {
    return Some("fullscreen");
}
```

**Step 3: Update DIFF_KEYS and MERGE_KEYS**

In `src/ui/diff_view.rs`:
```rust
static DIFF_KEYS: KeyBindings = KeyBindings {
    alt_left: "copy-chunk-right-left",
    alt_right: "copy-chunk-left-right",
    alt_shift_left: "pull-chunk-from-left",
    alt_shift_right: "pull-chunk-from-right",
    extra_ctrl_shift: &[("export-patch", gtk4::gdk::Key::p, gtk4::gdk::Key::P)],
    extra_ctrl: &[],
};
```

In `src/ui/merge_view.rs`:
```rust
static MERGE_KEYS: KeyBindings = KeyBindings {
    alt_left: "copy-chunk-right-middle",
    alt_right: "copy-chunk-left-middle",
    alt_shift_left: "pull-chunk-from-left",
    alt_shift_right: "pull-chunk-from-right",
    extra_ctrl_shift: &[],
    extra_ctrl: &[
        ("prev-conflict", gtk4::gdk::Key::j, gtk4::gdk::Key::J),
        ("next-conflict", gtk4::gdk::Key::k, gtk4::gdk::Key::K),
    ],
};
```

**Step 4: Build and fix any compile errors**

Run: `cargo build 2>&1 | head -30`

**Step 5: Commit**

```
git add src/ui/common/helpers.rs src/ui/diff_view.rs src/ui/merge_view.rs
git commit -m "Add key mappings for pane switch, pull change, delete change, fullscreen"
```

---

### Task 2: Add pane switching actions (diff_view.rs)

**Files:**
- Modify: `src/ui/diff_view.rs` — add actions after the existing copy-chunk actions (~line 844)

**Step 1: Add prev-pane and next-pane actions**

Insert after the `copy-chunk-left-right` action block (after line 844):

```rust
// Alt+PageUp: switch to previous pane (wraps)
{
    let action = gio::SimpleAction::new("prev-pane", None);
    let av = active_view.clone();
    let ltv = left_pane.text_view.clone();
    let rtv = right_pane.text_view.clone();
    action.connect_activate(move |_, _| {
        let active = av.borrow().clone();
        if active == ltv {
            rtv.grab_focus(); // wrap: left -> right
        } else {
            ltv.grab_focus();
        }
    });
    action_group.add_action(&action);
}
// Alt+PageDown: switch to next pane (wraps)
{
    let action = gio::SimpleAction::new("next-pane", None);
    let av = active_view.clone();
    let ltv = left_pane.text_view.clone();
    let rtv = right_pane.text_view.clone();
    action.connect_activate(move |_, _| {
        let active = av.borrow().clone();
        if active == ltv {
            rtv.grab_focus();
        } else {
            ltv.grab_focus(); // wrap: right -> left
        }
    });
    action_group.add_action(&action);
}
```

**Step 2: Build and verify**

Run: `cargo build 2>&1 | head -20`

**Step 3: Commit**

```
git add src/ui/diff_view.rs
git commit -m "Add Alt+PageUp/Down pane switching in file diff"
```

---

### Task 3: Add pane switching actions (merge_view.rs)

**Files:**
- Modify: `src/ui/merge_view.rs` — add actions near the other keyboard actions

**Step 1: Add prev-pane and next-pane actions**

Insert after the existing copy-chunk actions in `build_merge_view()`:

```rust
// Alt+PageUp: switch to previous pane (wraps: left->right->middle->left)
{
    let action = gio::SimpleAction::new("prev-pane", None);
    let av = active_view.clone();
    let ltv = left_pane.text_view.clone();
    let mtv = middle_pane.text_view.clone();
    let rtv = right_pane.text_view.clone();
    action.connect_activate(move |_, _| {
        let active = av.borrow().clone();
        if active == ltv {
            rtv.grab_focus(); // wrap
        } else if active == mtv {
            ltv.grab_focus();
        } else {
            mtv.grab_focus();
        }
    });
    action_group.add_action(&action);
}
// Alt+PageDown: switch to next pane (wraps: left->middle->right->left)
{
    let action = gio::SimpleAction::new("next-pane", None);
    let av = active_view.clone();
    let ltv = left_pane.text_view.clone();
    let mtv = middle_pane.text_view.clone();
    let rtv = right_pane.text_view.clone();
    action.connect_activate(move |_, _| {
        let active = av.borrow().clone();
        if active == ltv {
            mtv.grab_focus();
        } else if active == mtv {
            rtv.grab_focus();
        } else {
            ltv.grab_focus(); // wrap
        }
    });
    action_group.add_action(&action);
}
```

**Step 2: Build and verify**

Run: `cargo build 2>&1 | head -20`

**Step 3: Commit**

```
git add src/ui/merge_view.rs
git commit -m "Add Alt+PageUp/Down pane switching in merge view"
```

---

### Task 4: Add pull-chunk actions (diff_view.rs)

**Files:**
- Modify: `src/ui/diff_view.rs` — add after pane switching actions

**Step 1: Add pull-chunk-from-left and pull-chunk-from-right actions**

Pull is the reverse of push: it copies content from the adjacent pane INTO the focused pane.

- `pull-chunk-from-right` (Alt+Shift+Right): if focused on left pane, copy right→left. If focused on right, no-op (can't pull from further right).
- `pull-chunk-from-left` (Alt+Shift+Left): if focused on right pane, copy left→right. If focused on left, no-op.

Wait — Meld's logic: Alt+Shift+Right means "pull from right" regardless of focus. The content from the right side replaces the current chunk in the focused pane. So:

- `pull-chunk-from-right`: copy right chunk content into the focused pane's chunk range
- `pull-chunk-from-left`: copy left chunk content into the focused pane's chunk range

```rust
// Alt+Shift+Right: pull chunk from right into focused pane
{
    let action = gio::SimpleAction::new("pull-chunk-from-right", None);
    let av = active_view.clone();
    let ltv = left_pane.text_view.clone();
    let ch = chunks.clone();
    let cur = current_chunk.clone();
    let lb = left_buf.clone();
    let rb = right_buf.clone();
    action.connect_activate(move |_, _| {
        if let Some(idx) = cur.get() {
            let snapshot = ch.borrow();
            if let Some(c) = snapshot.get(idx) {
                let active = av.borrow().clone();
                if active == ltv {
                    // Focused on left: pull right content into left
                    copy_chunk(&rb, c.start_b, c.end_b, &lb, c.start_a, c.end_a);
                } else {
                    // Focused on right: right is already "from right" — no-op
                    // (same as copy-chunk-right-left from the other perspective)
                }
            }
        }
    });
    action_group.add_action(&action);
}
// Alt+Shift+Left: pull chunk from left into focused pane
{
    let action = gio::SimpleAction::new("pull-chunk-from-left", None);
    let av = active_view.clone();
    let ltv = left_pane.text_view.clone();
    let ch = chunks.clone();
    let cur = current_chunk.clone();
    let lb = left_buf.clone();
    let rb = right_buf.clone();
    action.connect_activate(move |_, _| {
        if let Some(idx) = cur.get() {
            let snapshot = ch.borrow();
            if let Some(c) = snapshot.get(idx) {
                let active = av.borrow().clone();
                if active == ltv {
                    // Focused on left: left is already "from left" — no-op
                } else {
                    // Focused on right: pull left content into right
                    copy_chunk(&lb, c.start_a, c.end_a, &rb, c.start_b, c.end_b);
                }
            }
        }
    });
    action_group.add_action(&action);
}
```

**Step 2: Build and verify**

Run: `cargo build 2>&1 | head -20`

**Step 3: Commit**

```
git add src/ui/diff_view.rs
git commit -m "Add Alt+Shift+Left/Right pull chunk actions in file diff"
```

---

### Task 5: Add pull-chunk actions (merge_view.rs)

**Files:**
- Modify: `src/ui/merge_view.rs`

**Step 1: Add pull-chunk actions for merge view**

In merge view, pull copies FROM the specified side INTO the middle pane (since left/right are read-only, pulling only makes sense when focused on middle):

```rust
// Alt+Shift+Right: pull from right into middle
{
    let action = gio::SimpleAction::new("pull-chunk-from-right", None);
    let av = active_view.clone();
    let mtv = middle_pane.text_view.clone();
    let lch = left_chunks.clone();
    let rch = right_chunks.clone();
    let mb = middle_buf.clone();
    let rb = right_buf.clone();
    action.connect_activate(move |_, _| {
        let active = av.borrow().clone();
        if active != mtv {
            return; // Only middle pane can receive pulls
        }
        let cursor_line = {
            let iter = mb.iter_at_mark(&mb.get_insert());
            iter.line() as usize
        };
        // Find which right chunk contains the cursor in middle
        for mc in rch.borrow().iter() {
            if mc.tag == DiffTag::Equal {
                continue;
            }
            if cursor_line >= mc.start_a && cursor_line < mc.end_a.max(mc.start_a + 1) {
                copy_chunk(&rb, mc.start_b, mc.end_b, &mb, mc.start_a, mc.end_a);
                return;
            }
        }
    });
    action_group.add_action(&action);
}
// Alt+Shift+Left: pull from left into middle
{
    let action = gio::SimpleAction::new("pull-chunk-from-left", None);
    let av = active_view.clone();
    let mtv = middle_pane.text_view.clone();
    let lch = left_chunks.clone();
    let rch = right_chunks.clone();
    let mb = middle_buf.clone();
    let lb = left_buf.clone();
    action.connect_activate(move |_, _| {
        let active = av.borrow().clone();
        if active != mtv {
            return;
        }
        let cursor_line = {
            let iter = mb.iter_at_mark(&mb.get_insert());
            iter.line() as usize
        };
        for mc in lch.borrow().iter() {
            if mc.tag == DiffTag::Equal {
                continue;
            }
            if cursor_line >= mc.start_b && cursor_line < mc.end_b.max(mc.start_b + 1) {
                copy_chunk(&lb, mc.start_a, mc.end_a, &mb, mc.start_b, mc.end_b);
                return;
            }
        }
    });
    action_group.add_action(&action);
}
```

**Step 2: Build and verify**

Run: `cargo build 2>&1 | head -20`

**Step 3: Commit**

```
git add src/ui/merge_view.rs
git commit -m "Add Alt+Shift+Left/Right pull chunk actions in merge view"
```

---

### Task 6: Add delete-chunk action (diff_view.rs)

**Files:**
- Modify: `src/ui/diff_view.rs`
- Modify: `src/ui/common/gutter.rs` — add `delete_chunk` helper function

**Step 1: Add `delete_chunk` helper in gutter.rs**

Add after the existing `copy_chunk` function:

```rust
/// Delete a chunk's text from the buffer (Meld's "delete change").
pub fn delete_chunk(buf: &TextBuffer, start: usize, end: usize) {
    let mut start_iter = buf
        .iter_at_line(start as i32)
        .unwrap_or(buf.start_iter());

    let mut end_iter = if (end as i32) < buf.line_count() {
        buf.iter_at_line(end as i32)
            .unwrap_or(buf.end_iter())
    } else {
        // Chunk extends to end of buffer — also remove preceding newline
        let mut it = buf.end_iter();
        if it.backward_char() && it.char() == '\n' && start > 0 {
            start_iter = buf
                .iter_at_line(start as i32 - 1)
                .unwrap_or(buf.start_iter());
            if !start_iter.ends_line() {
                start_iter.forward_to_line_end();
            }
        }
        buf.end_iter()
    };

    buf.begin_user_action();
    buf.delete(&mut start_iter, &mut end_iter);
    buf.end_user_action();
    buf.place_cursor(&start_iter);
}
```

**Step 2: Add delete-chunk action in diff_view.rs**

```rust
// Alt+Delete: delete current chunk from focused pane
{
    let action = gio::SimpleAction::new("delete-chunk", None);
    let av = active_view.clone();
    let ltv = left_pane.text_view.clone();
    let ch = chunks.clone();
    let cur = current_chunk.clone();
    let lb = left_buf.clone();
    let rb = right_buf.clone();
    action.connect_activate(move |_, _| {
        if let Some(idx) = cur.get() {
            let snapshot = ch.borrow();
            if let Some(c) = snapshot.get(idx) {
                let active = av.borrow().clone();
                if active == ltv {
                    delete_chunk(&lb, c.start_a, c.end_a);
                } else {
                    delete_chunk(&rb, c.start_b, c.end_b);
                }
            }
        }
    });
    action_group.add_action(&action);
}
```

**Step 3: Build and verify**

Run: `cargo build 2>&1 | head -20`

**Step 4: Commit**

```
git add src/ui/common/gutter.rs src/ui/diff_view.rs
git commit -m "Add Alt+Delete to delete current chunk from focused pane"
```

---

### Task 7: Add delete-chunk action (merge_view.rs)

**Files:**
- Modify: `src/ui/merge_view.rs`

**Step 1: Add delete-chunk action**

In merge view, delete only works on the middle (editable) pane:

```rust
// Alt+Delete: delete current chunk from middle pane
{
    let action = gio::SimpleAction::new("delete-chunk", None);
    let av = active_view.clone();
    let mtv = middle_pane.text_view.clone();
    let lch = left_chunks.clone();
    let rch = right_chunks.clone();
    let mb = middle_buf.clone();
    action.connect_activate(move |_, _| {
        let active = av.borrow().clone();
        if active != mtv {
            return; // Only middle pane supports delete
        }
        let cursor_line = {
            let iter = mb.iter_at_mark(&mb.get_insert());
            iter.line() as usize
        };
        // Check left_chunks (middle = B side)
        for mc in lch.borrow().iter() {
            if mc.tag == DiffTag::Equal {
                continue;
            }
            if cursor_line >= mc.start_b && cursor_line < mc.end_b.max(mc.start_b + 1) {
                delete_chunk(&mb, mc.start_b, mc.end_b);
                return;
            }
        }
        // Check right_chunks (middle = A side)
        for mc in rch.borrow().iter() {
            if mc.tag == DiffTag::Equal {
                continue;
            }
            if cursor_line >= mc.start_a && cursor_line < mc.end_a.max(mc.start_a + 1) {
                delete_chunk(&mb, mc.start_a, mc.end_a);
                return;
            }
        }
    });
    action_group.add_action(&action);
}
```

**Step 2: Build and verify**

Run: `cargo build 2>&1 | head -20`

**Step 3: Commit**

```
git add src/ui/merge_view.rs
git commit -m "Add Alt+Delete to delete chunk in merge view middle pane"
```

---

### Task 8: Add F11 fullscreen toggle

**Files:**
- Modify: `src/ui/common/tabs.rs:624-696` — add fullscreen action in `build_app_window()`

**Step 1: Add fullscreen action to win_actions**

Insert in the `build_app_window` function, inside the win_actions block (after the `new-comparison` action, ~line 657):

```rust
{
    let action = gio::SimpleAction::new("fullscreen", None);
    let w = window.clone();
    action.connect_activate(move |_, _| {
        if w.is_fullscreen() {
            w.unfullscreen();
        } else {
            w.fullscreen();
        }
    });
    win_actions.add_action(&action);
}
```

**Step 2: Register F11 accelerator**

In the keyboard accelerators block (~line 695), add:

```rust
gtk_app.set_accels_for_action("win.fullscreen", &["F11"]);
```

**Step 3: Also handle F11 in `map_key_to_action` so it works when a source view has focus**

The F11 mapping was already added in Task 1. But since the action is on the `win` group (not `diff`), the `map_key_to_action` return of `"fullscreen"` won't find it in the diff action group. Two options:

Option A: Remove F11 from `map_key_to_action()` and rely solely on the GTK accelerator (the accel registered in tabs.rs will fire regardless of focus).

This is the simplest — **remove the F11 mapping from `map_key_to_action()`** since the GTK accelerator system handles it at the application level.

**Step 4: Build and verify**

Run: `cargo build 2>&1 | head -20`

**Step 5: Commit**

```
git add src/ui/common/tabs.rs src/ui/common/helpers.rs
git commit -m "Add F11 fullscreen toggle to all window types"
```

---

### Task 9: Register accelerators for new actions

**Files:**
- Modify: `src/ui/common/tabs.rs:668-696` — add accel registrations

**Step 1: Add accelerator registrations**

In the keyboard accelerators block in `build_app_window()`, add:

```rust
gtk_app.set_accels_for_action("diff.prev-pane", &["<Alt>Page_Up"]);
gtk_app.set_accels_for_action("diff.next-pane", &["<Alt>Page_Down"]);
gtk_app.set_accels_for_action("diff.pull-chunk-from-left", &["<Alt><Shift>Left"]);
gtk_app.set_accels_for_action("diff.pull-chunk-from-right", &["<Alt><Shift>Right"]);
gtk_app.set_accels_for_action("diff.delete-chunk", &["<Alt>Delete"]);
```

**Step 2: Build and verify**

Run: `cargo build 2>&1 | head -20`

**Step 3: Commit**

```
git add src/ui/common/tabs.rs
git commit -m "Register accelerators for pane switch, pull, delete actions"
```

---

### Task 10: Run clippy and fmt

**Step 1: Format**

Run: `cargo +nightly fmt`

**Step 2: Clippy**

Run: `cargo clippy -- -W clippy::pedantic 2>&1 | head -40`

Fix any warnings.

**Step 3: Commit**

```
git add -u
git commit -m "Fix clippy and formatting"
```

---

### Task 11: Manual test and update docs

**Step 1: Build release for Docker testing**

Run: `cargo build --release`

**Step 2: Test in Docker**

Copy binary and run UI tests to verify nothing is broken:
```
docker cp target/release/mergers ubuntu:/root/mergers/target/release/mergers
docker exec ubuntu bash -c "cd /root/mergers && dbus-run-session -- xvfb-run -a tests/ui_integration/.venv/bin/python3 -m pytest tests/ui_integration/ -v --tb=short"
```

**Step 3: Update TESTING.md**

Mark the newly implemented items as done:
- `[x] Pane switching (Alt+PageUp/Down)`
- `[x] Pull change from left/right (Alt+Shift+Left/Right)`
- `[x] Delete change (Alt+Delete)`
- `[x] Fullscreen (F11)`

**Step 4: Update meld-feature-gaps.md**

Mark these items as completed in the memory file.

**Step 5: Commit**

```
git add TESTING.md
git commit -m "Update TESTING.md with new keyboard shortcuts"
```

---

### Task 12: Write UI integration tests for new features

**Files:**
- Modify: `tests/ui_integration/test_file_diff_unique.py`

**Step 1: Add pane switching test**

```python
def test_alt_page_down_switches_pane(app_process, fixture_path):
    """Alt+PageDown should switch focus to the other pane."""
    proc = app_process(fixture_path("left.txt"), fixture_path("right.txt"))
    app = find_app()
    doDelay(1)

    # Should start focused on left pane
    send_keys("alt+Next", proc.pid)  # Next = PageDown in xdotool
    doDelay(0.5)

    # App should still be responsive (didn't crash)
    frames = app.findChildren(lambda n: n.roleName == "frame")
    assert len(frames) >= 1, "Window disappeared after pane switch"


def test_alt_page_up_switches_pane(app_process, fixture_path):
    """Alt+PageUp should switch focus to the other pane."""
    proc = app_process(fixture_path("left.txt"), fixture_path("right.txt"))
    app = find_app()
    doDelay(1)

    send_keys("alt+Prior", proc.pid)  # Prior = PageUp in xdotool
    doDelay(0.5)

    frames = app.findChildren(lambda n: n.roleName == "frame")
    assert len(frames) >= 1, "Window disappeared after pane switch"
```

**Step 2: Add delete chunk test**

```python
def test_alt_delete_removes_chunk(app_process, fixture_path):
    """Alt+Delete should delete the current chunk from the focused pane."""
    import tempfile, shutil, os
    from conftest import FIXTURES
    with tempfile.TemporaryDirectory() as tmpdir:
        left = os.path.join(tmpdir, "left.txt")
        right = os.path.join(tmpdir, "right.txt")
        shutil.copy2(os.path.join(FIXTURES, "left.txt"), left)
        shutil.copy2(os.path.join(FIXTURES, "right.txt"), right)

        proc = app_process(left, right)
        app = find_app()
        doDelay(1)

        # Navigate to first chunk
        send_keys("alt+Down", proc.pid)
        doDelay(0.5)

        # Delete the chunk
        send_keys("alt+Delete", proc.pid)
        doDelay(1)

        # Save button should be sensitive (buffer was modified)
        save_buttons = app.findChildren(
            lambda n: n.roleName == "push button" and "Save" in n.name
        )
        assert any(b.sensitive for b in save_buttons), \
            "Save button should be sensitive after deleting a chunk"
```

**Step 3: Add fullscreen test**

```python
def test_f11_toggles_fullscreen(app_process, fixture_path):
    """F11 should toggle fullscreen mode."""
    proc = app_process(fixture_path("left.txt"), fixture_path("right.txt"))
    app = find_app()
    doDelay(1)

    send_keys("F11", proc.pid)
    doDelay(1)

    # App should still be responsive
    frames = app.findChildren(lambda n: n.roleName == "frame")
    assert len(frames) >= 1, "Window disappeared after F11"

    # Toggle back
    send_keys("F11", proc.pid)
    doDelay(1)

    frames = app.findChildren(lambda n: n.roleName == "frame")
    assert len(frames) >= 1, "Window disappeared after second F11"
```

**Step 4: Run tests in Docker to verify**

**Step 5: Commit**

```
git add tests/ui_integration/test_file_diff_unique.py
git commit -m "Add UI tests for pane switching, delete chunk, and fullscreen"
```
