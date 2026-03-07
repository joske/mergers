"""CLI argument handling tests (non-GUI, no AT-SPI needed)."""
import os
import subprocess

from conftest import MERGERS_BIN


def test_nonexistent_path_exits_nonzero():
    """Passing a non-existent path should exit with non-zero status."""
    result = subprocess.run(
        [MERGERS_BIN, "/tmp/does_not_exist_abc123", "/tmp/also_missing_xyz789"],
        capture_output=True,
        timeout=10,
    )
    assert result.returncode != 0, (
        f"Expected non-zero exit for non-existent paths, got {result.returncode}"
    )
    stderr = result.stderr.decode()
    assert "Error" in stderr or "error" in stderr, (
        f"Expected error message on stderr, got: {stderr}"
    )


def test_single_file_exits_nonzero(fixture_path):
    """Passing a single non-directory file should exit with non-zero status."""
    path = fixture_path("left.txt")
    result = subprocess.run(
        [MERGERS_BIN, path],
        capture_output=True,
        timeout=10,
    )
    assert result.returncode != 0, (
        f"Expected non-zero exit for single file arg, got {result.returncode}"
    )
    stderr = result.stderr.decode()
    assert "single file" in stderr.lower() or "error" in stderr.lower(), (
        f"Expected error about single file, got: {stderr}"
    )


def test_version_flag():
    """--version should output a version string and exit 0."""
    result = subprocess.run(
        [MERGERS_BIN, "--version"],
        capture_output=True,
        timeout=10,
    )
    assert result.returncode == 0, (
        f"Expected exit 0 for --version, got {result.returncode}"
    )
    stdout = result.stdout.decode().strip()
    # clap outputs "mergers <version>"
    assert "mergers" in stdout.lower(), (
        f"Expected 'mergers' in version output, got: {stdout}"
    )
    # Should contain a version number (digit.digit pattern)
    assert any(c.isdigit() for c in stdout), (
        f"Expected version number in output, got: {stdout}"
    )


def test_help_flag():
    """--help should output help text and exit 0."""
    result = subprocess.run(
        [MERGERS_BIN, "--help"],
        capture_output=True,
        timeout=10,
    )
    assert result.returncode == 0, (
        f"Expected exit 0 for --help, got {result.returncode}"
    )
    stdout = result.stdout.decode()
    assert "usage" in stdout.lower() or "paths" in stdout.lower(), (
        f"Expected usage/help text, got: {stdout}"
    )
