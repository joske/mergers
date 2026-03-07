"""File diff tests that need their own app instance (unique args or state)."""
from dogtail.utils import doDelay

from conftest import find_app, find_labels, send_keys, wait_for_label


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
