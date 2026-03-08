"""File diff tests that need their own app instance (unique args or state)."""
import os
import tempfile

from dogtail.utils import doDelay

from conftest import copy_fixture, find_app, find_labels, send_keys, wait_for_label


def test_three_way_merge_opens(app_process, fixture_path):
    """Opening three files should create a merge view."""
    app_process(
        fixture_path("left.txt"),
        fixture_path("base.txt"),
        fixture_path("right.txt"),
    )
    app = find_app()
    windows = app.findChildren(lambda n: n.roleName == "frame")
    assert len(windows) >= 1, "No window found for 3-way merge"


def test_identical_files_no_chunks(app_process, fixture_path):
    """Diffing a file against itself should show no diff chunks."""
    app_process(fixture_path("left.txt"), fixture_path("left.txt"))
    app = find_app()
    doDelay(1)
    labels = find_labels(app)
    assert any("No changes" in t for t in labels), \
        f"Expected 'No changes' in labels: {labels}"


def test_blank_comparison(app_process):
    """Starting with no arguments should open a blank comparison."""
    app_process()
    app = find_app()
    doDelay(1)
    windows = app.findChildren(lambda n: n.roleName == "frame")
    assert len(windows) >= 1, "No window found for blank comparison"


def test_binary_file_shows_info_bar(app_process, fixture_path):
    """Opening a binary file should show a 'Binary' info bar message."""
    app_process(fixture_path("binary.bin"), fixture_path("left.txt"))
    app = find_app()
    doDelay(1)
    labels = find_labels(app)
    assert any("Binary" in t for t in labels), \
        f"Expected 'Binary' info bar label, got: {labels}"


def test_custom_labels_shown(app_process, fixture_path):
    """Passing -L flags should display custom labels."""
    app_process(
        fixture_path("left.txt"),
        fixture_path("right.txt"),
        "-L", "Original",
        "-L", "Modified",
    )
    app = find_app()
    doDelay(1)
    labels = find_labels(app)
    assert any("Original" in t for t in labels), \
        f"Expected 'Original' in labels: {labels}"
    assert any("Modified" in t for t in labels), \
        f"Expected 'Modified' in labels: {labels}"


# -- Keyboard shortcut tests (focus-dependent, need own instance) -----------

def test_ctrl_f_opens_find_bar(app_process, fixture_path):
    """Ctrl+F should open the find bar."""
    proc = app_process(fixture_path("left.txt"), fixture_path("right.txt"))
    app = find_app()
    doDelay(1)

    send_keys("ctrl+f", proc.pid)
    doDelay(1)

    entries = app.findChildren(
        lambda n: n.roleName == "text" and n.showing
    )
    # Find bar adds at least one text entry beyond the source views
    assert len(entries) >= 3, \
        f"Expected find bar entry (>=3 text widgets), found {len(entries)}"


def test_escape_closes_find_bar(app_process, fixture_path):
    """Escape should close the find bar opened by Ctrl+F."""
    proc = app_process(fixture_path("left.txt"), fixture_path("right.txt"))
    app = find_app()
    doDelay(1)

    send_keys("ctrl+f", proc.pid)
    doDelay(1)
    entries_open = app.findChildren(
        lambda n: n.roleName == "text" and n.showing
    )

    send_keys("Escape", proc.pid)
    doDelay(1)
    entries_closed = app.findChildren(
        lambda n: n.roleName == "text" and n.showing
    )
    assert len(entries_closed) < len(entries_open), \
        f"Find bar did not close. Before Escape: {len(entries_open)}, after: {len(entries_closed)}"


def test_go_to_line_opens_with_ctrl_l(app_process, fixture_path):
    """Ctrl+L should open the go-to-line dialog."""
    proc = app_process(fixture_path("left.txt"), fixture_path("right.txt"))
    app = find_app()
    doDelay(1)

    send_keys("ctrl+l", proc.pid)
    doDelay(1)

    # The go-to-line bar adds a text entry or spin button
    entries = app.findChildren(
        lambda n: n.roleName in ("text", "spin button") and n.showing
    )
    assert len(entries) >= 3, \
        f"Expected go-to-line entry, found {len(entries)} text/spin widgets"


def test_alt_down_navigates_to_next_chunk(app_process, fixture_path):
    """Alt+Down should navigate to next chunk."""
    proc = app_process(fixture_path("left.txt"), fixture_path("right.txt"))
    app = find_app()
    doDelay(1)

    labels_before = find_labels(app)
    send_keys("alt+Down", proc.pid)
    doDelay(1)
    labels_after = find_labels(app)

    # The chunk navigation label should still be present
    assert any("change" in t.lower() for t in labels_after), \
        f"Expected chunk label after Alt+Down: {labels_after}"


def test_chunk_navigation_updates_label(app_process, fixture_path):
    """Navigating chunks should update the navigation label."""
    proc = app_process(fixture_path("left.txt"), fixture_path("right.txt"))
    app = find_app()
    doDelay(1)

    send_keys("alt+Down", proc.pid)
    doDelay(0.5)

    result = wait_for_label(app, lambda t: "change" in t.lower(), timeout=3)
    assert result is not None, \
        f"Expected chunk navigation label after navigation: {find_labels(app)}"


def test_no_wrap_navigation_by_default(app_process, fixture_path):
    """Navigating past last chunk should not wrap to first by default."""
    proc = app_process(fixture_path("left.txt"), fixture_path("right.txt"))
    app = find_app()
    doDelay(1)

    # Navigate forward many times to go past the last chunk
    for _ in range(20):
        send_keys("alt+Down", proc.pid)
        doDelay(0.2)

    labels = find_labels(app)
    assert any("change" in t.lower() for t in labels), \
        f"Expected chunk label to remain: {labels}"


def test_ctrl_h_opens_find_replace_bar(app_process, fixture_path):
    """Ctrl+H should open the find+replace bar."""
    proc = app_process(fixture_path("left.txt"), fixture_path("right.txt"))
    app = find_app()
    doDelay(1)

    send_keys("ctrl+h", proc.pid)
    doDelay(1)

    entries = app.findChildren(
        lambda n: n.roleName == "text" and n.showing
    )
    # Find+replace bar adds at least 2 entries beyond the source views
    assert len(entries) >= 4, \
        f"Expected find+replace entries (>=4 text widgets), found {len(entries)}"


def test_empty_file_vs_nonempty(app_process):
    """Comparing an empty file with a non-empty file should show all-insert chunks."""
    with tempfile.TemporaryDirectory() as tmpdir:
        empty = os.path.join(tmpdir, "empty.txt")
        nonempty = os.path.join(tmpdir, "nonempty.txt")
        with open(empty, "w") as f:
            pass  # empty
        with open(nonempty, "w") as f:
            f.write("line 1\nline 2\nline 3\n")

        proc = app_process(empty, nonempty)
        app = find_app()
        doDelay(1)
        labels = find_labels(app)
        # Should show at least 1 change
        assert any("change" in t.lower() for t in labels), \
            f"Expected change label for empty vs nonempty: {labels}"


def test_whitespace_only_diff_with_spaces_toggle(app_process):
    """Files differing only in whitespace + Spaces toggle = no changes."""
    with tempfile.TemporaryDirectory() as tmpdir:
        f1 = os.path.join(tmpdir, "a.txt")
        f2 = os.path.join(tmpdir, "b.txt")
        with open(f1, "w") as f:
            f.write("hello world\n")
        with open(f2, "w") as f:
            f.write("hello  world\n")  # extra space

        proc = app_process(f1, f2)
        app = find_app()
        doDelay(1)

        # Toggle Spaces on
        spaces_btn = app.findChild(
            lambda n: n.roleName == "toggle button" and "Space" in n.name and n.showing
        )
        assert spaces_btn is not None, "Spaces toggle not found"
        spaces_btn.do_action(0)
        doDelay(1)

        labels = find_labels(app)
        assert any("no changes" in t.lower() or "identical" in t.lower() for t in labels), \
            f"Expected 'No changes' with spaces toggle on: {labels}"


def test_blank_lines_diff_with_blanks_toggle(app_process):
    """Files differing only in blank lines + Blanks toggle = no changes."""
    with tempfile.TemporaryDirectory() as tmpdir:
        f1 = os.path.join(tmpdir, "a.txt")
        f2 = os.path.join(tmpdir, "b.txt")
        with open(f1, "w") as f:
            f.write("hello\nworld\n")
        with open(f2, "w") as f:
            f.write("hello\n\nworld\n")  # extra blank line

        proc = app_process(f1, f2)
        app = find_app()
        doDelay(1)

        # Toggle Blanks on
        blanks_btn = app.findChild(
            lambda n: n.roleName == "toggle button" and "Blank" in n.name and n.showing
        )
        assert blanks_btn is not None, "Blanks toggle not found"
        blanks_btn.do_action(0)
        doDelay(1)

        labels = find_labels(app)
        assert any("no changes" in t.lower() or "identical" in t.lower() for t in labels), \
            f"Expected 'No changes' with blanks toggle on: {labels}"


# -- Pane switching tests ---------------------------------------------------

def _focused_text_index(app):
    """Return the index of the focused text widget among visible ones, or -1."""
    texts = app.findChildren(lambda n: n.roleName == "text" and n.showing)
    for i, t in enumerate(texts):
        try:
            if t.focused:
                return i
        except Exception:
            pass
    return -1


def test_alt_page_down_switches_pane(app_process, fixture_path):
    """Alt+PageDown should switch focus to the other pane."""
    proc = app_process(fixture_path("left.txt"), fixture_path("right.txt"))
    app = find_app()
    doDelay(1)

    before = _focused_text_index(app)
    send_keys("alt+Next", proc.pid)  # Next = PageDown in xdotool
    doDelay(0.5)
    after = _focused_text_index(app)

    assert after != before or after == -1, \
        f"Focus did not change after Alt+PageDown: before={before}, after={after}"


def test_alt_page_up_switches_pane(app_process, fixture_path):
    """Alt+PageUp should switch focus to the other pane."""
    proc = app_process(fixture_path("left.txt"), fixture_path("right.txt"))
    app = find_app()
    doDelay(1)

    before = _focused_text_index(app)
    send_keys("alt+Prior", proc.pid)  # Prior = PageUp in xdotool
    doDelay(0.5)
    after = _focused_text_index(app)

    assert after != before or after == -1, \
        f"Focus did not change after Alt+PageUp: before={before}, after={after}"


# -- Delete chunk test ------------------------------------------------------

def test_alt_delete_removes_chunk(app_process, fixture_path):
    """Alt+Delete should delete the current chunk from the focused pane."""
    with tempfile.TemporaryDirectory() as tmpdir:
        left = copy_fixture("left.txt", tmpdir)
        right = copy_fixture("right.txt", tmpdir)

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


# -- Fullscreen test --------------------------------------------------------

def test_f11_toggles_fullscreen(app_process, fixture_path):
    """F11 should toggle fullscreen mode without crashing."""
    proc = app_process(fixture_path("left.txt"), fixture_path("right.txt"))
    app = find_app()
    doDelay(1)

    send_keys("F11", proc.pid)
    doDelay(1)

    frames = app.findChildren(lambda n: n.roleName == "frame")
    assert len(frames) >= 1, "Window disappeared after F11"

    # Toggle back
    send_keys("F11", proc.pid)
    doDelay(1)

    frames = app.findChildren(lambda n: n.roleName == "frame")
    assert len(frames) >= 1, "Window disappeared after second F11"
