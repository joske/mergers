"""File diff tests that share a single mergers instance (left.txt vs right.txt).

All tests here are read-only or leave reversible state, so they can safely
share one app process to avoid repeated startup costs.
"""
from conftest import find_labels, send_keys, send_keys_until, wait_for_label


# -- Display & labels -------------------------------------------------------

def test_file_diff_opens(shared_diff_app):
    """Window should be accessible via AT-SPI."""
    _, app = shared_diff_app
    windows = app.findChildren(lambda n: n.roleName == "frame")
    assert len(windows) >= 1, "No window found"


def test_save_button_initially_insensitive(shared_diff_app):
    """Save buttons should be insensitive when no changes have been made."""
    _, app = shared_diff_app
    buttons = app.findChildren(
        lambda n: n.roleName == "push button" and "Save" in n.name
    )
    for btn in buttons:
        assert not btn.sensitive, f"Save button '{btn.name}' should be insensitive initially"


def test_diff_shows_file_names(shared_diff_app):
    """The diff window should show the file names being compared."""
    _, app = shared_diff_app
    labels = find_labels(app)
    assert any("left.txt" in t for t in labels), \
        f"Expected 'left.txt' in labels: {labels}"
    assert any("right.txt" in t for t in labels), \
        f"Expected 'right.txt' in labels: {labels}"


def test_chunk_navigation_label(shared_diff_app):
    """Diff with changes should show chunk count in navigation label."""
    _, app = shared_diff_app
    labels = find_labels(app)
    assert any("changes" in t for t in labels), \
        f"Expected chunk count label with 'changes' in labels: {labels}"


def test_window_title_is_mergers(shared_diff_app):
    """Window title should be 'Mergers'."""
    _, app = shared_diff_app
    frames = app.findChildren(lambda n: n.roleName == "frame")
    assert frames, "No frame found"
    assert "ergers" in frames[0].name, f"Expected 'Mergers' in title, got '{frames[0].name}'"


# -- Swap panes (reversible) ------------------------------------------------

def test_swap_panes(shared_diff_app):
    """Swap panes should reverse the file path labels."""
    _, app = shared_diff_app
    labels_before = find_labels(app)
    path_labels_before = [t for t in labels_before if "fixtures/" in t]
    assert len(path_labels_before) >= 2, \
        f"Expected at least 2 path labels, got: {labels_before}"

    swap_btn = app.findChild(
        lambda n: n.roleName == "push button" and n.name == "Swap panes"
    )
    assert swap_btn is not None, "Swap panes button not found"
    swap_btn.do_action(0)

    result = wait_for_label(app, lambda t: "fixtures/" in t, timeout=2)
    labels_after = find_labels(app)
    path_labels_after = [t for t in labels_after if "fixtures/" in t]
    assert path_labels_after != path_labels_before, \
        f"Path labels did not change after swap: {path_labels_after}"

    # Swap back to restore state for other tests
    swap_btn.do_action(0)
    wait_for_label(app, lambda t: "fixtures/" in t, timeout=2)


def test_window_title_unchanged_after_swap(shared_diff_app):
    """Window title should stay 'Mergers' after swap."""
    _, app = shared_diff_app
    frames = app.findChildren(lambda n: n.roleName == "frame")
    assert "ergers" in frames[0].name


# -- Toggle buttons (reversible) --------------------------------------------

def test_blanks_toggle(shared_diff_app):
    """Blanks toggle should not break the chunk label."""
    _, app = shared_diff_app
    blanks_btn = app.findChild(
        lambda n: n.roleName == "toggle button" and "Blank" in n.name and n.showing
    )
    assert blanks_btn is not None, "Blanks toggle button not found"
    blanks_btn.do_action(0)
    result = wait_for_label(app, lambda t: "changes" in t.lower() or "no changes" in t.lower(), timeout=3)
    assert result is not None, f"Chunk label disappeared after Blanks toggle: {find_labels(app)}"
    # Toggle back
    blanks_btn.do_action(0)
    wait_for_label(app, lambda t: "changes" in t.lower(), timeout=2)


def test_spaces_toggle(shared_diff_app):
    """Spaces toggle should not break the chunk label."""
    _, app = shared_diff_app
    spaces_btn = app.findChild(
        lambda n: n.roleName == "toggle button" and "Space" in n.name and n.showing
    )
    assert spaces_btn is not None, "Spaces toggle button not found"
    spaces_btn.do_action(0)
    result = wait_for_label(app, lambda t: "changes" in t.lower() or "no changes" in t.lower(), timeout=3)
    assert result is not None, f"Chunk label disappeared after Spaces toggle: {find_labels(app)}"
    # Toggle back
    spaces_btn.do_action(0)
    wait_for_label(app, lambda t: "changes" in t.lower(), timeout=2)


# -- Keyboard shortcut tests need their own instance (focus-dependent) ------
# See test_file_diff_unique.py for: ctrl_f, escape, ctrl_l, ctrl_d, wrap nav
