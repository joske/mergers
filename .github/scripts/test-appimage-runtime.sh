#!/usr/bin/env bash
set -euo pipefail

# Run in a clean Ubuntu 24.04 environment with the extracted AppImage at /app.
/app/AppRun --version
printf 'fn main() {}\n' > /tmp/left.rs
printf 'fn main() { println!("hello"); }\n' > /tmp/right.rs
export GTK_A11Y=none GDK_BACKEND=x11 GSK_RENDERER=cairo
# Variables in this block belong to the inner shell.
# shellcheck disable=SC2016
dbus-run-session -- xvfb-run -a bash -euc '
    /app/AppRun /tmp/left.rs /tmp/right.rs > /tmp/mergers.log 2>&1 &
    app_pid=$!
    trap "kill $app_pid 2>/dev/null || true; cat /tmp/mergers.log" EXIT
    timeout 30 xdotool search --sync --onlyvisible --pid "$app_pid"
    # Catch crashes shortly after the first window appears, as the catalog does.
    sleep 12
    kill -0 "$app_pid"
    if grep -Ei "error while loading shared libraries|failed to load|could not find.*(language|style)|GLib-GIO-ERROR|Fontconfig error" /tmp/mergers.log; then
        exit 1
    fi
'
