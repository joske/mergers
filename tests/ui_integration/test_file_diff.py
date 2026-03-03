"""Smoke tests for file diff window via dogtail AT-SPI."""
from conftest import find_app


def test_file_diff_opens(app_process, fixture_path):
    """Open a two-file diff and verify the window is accessible via AT-SPI."""
    app_process(fixture_path("left.txt"), fixture_path("right.txt"))
    app = find_app()

    # Verify a window exists
    windows = app.findChildren(lambda n: n.roleName == "frame")
    assert len(windows) >= 1, "No window found"


def test_save_button_initially_insensitive(app_process, fixture_path):
    """Save buttons should be insensitive when no changes have been made."""
    app_process(fixture_path("left.txt"), fixture_path("right.txt"))
    app = find_app()
    buttons = app.findChildren(
        lambda n: n.roleName == "push button" and "Save" in n.name
    )
    for btn in buttons:
        assert not btn.sensitive, f"Save button '{btn.name}' should be insensitive initially"
