"""3-way merge view tests sharing a single mergers instance."""
from conftest import find_labels, send_keys_until


def test_merge_opens_with_three_panes(shared_merge_app):
    """3-way merge should have at least 3 text panes."""
    _, app = shared_merge_app
    text_views = app.findChildren(lambda n: n.roleName == "text" and n.showing)
    assert len(text_views) >= 3, \
        f"Expected at least 3 text panes, found {len(text_views)}"


def test_merge_shows_file_names(shared_merge_app):
    """Merge view should display all three file names."""
    _, app = shared_merge_app
    labels = find_labels(app)
    assert any("left.txt" in t for t in labels), f"Expected 'left.txt' in labels: {labels}"
    assert any("base.txt" in t for t in labels), f"Expected 'base.txt' in labels: {labels}"
    assert any("right.txt" in t for t in labels), f"Expected 'right.txt' in labels: {labels}"


def test_merge_window_title_contains_file_names(shared_merge_app):
    """Tab label should contain file names."""
    _, app = shared_merge_app
    labels = find_labels(app)
    tab_labels = [t for t in labels if "left.txt" in t and "right.txt" in t]
    assert tab_labels, f"Expected a tab label with both file names, got: {labels}"


def test_merge_has_one_save_button(shared_merge_app):
    """Only the middle pane should have a save button."""
    _, app = shared_merge_app
    save_buttons = app.findChildren(
        lambda n: n.roleName == "push button" and "Save" in n.name and n.showing
    )
    assert len(save_buttons) == 1, \
        f"Expected 1 save button, found {len(save_buttons)}: {[b.name for b in save_buttons]}"


def test_merge_chunk_navigation(shared_merge_app):
    """Ctrl+D in merge view should show a chunk navigation label."""
    proc, app = shared_merge_app
    chunk_text = send_keys_until(
        app, "ctrl+d", proc.pid,
        lambda t: "of" in t.lower() or "change" in t.lower(),
        retries=5, delay=2,
    )
    assert chunk_text is not None, \
        f"Expected chunk navigation label after Ctrl+D. Labels: {find_labels(app)}"
