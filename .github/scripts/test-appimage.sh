#!/usr/bin/env bash
set -euo pipefail

image=$(realpath "${1:?Usage: test-appimage.sh APPIMAGE}")
runtime_test=$(realpath "$(dirname "$0")/test-appimage-runtime.sh")
test_dir=$(mktemp -d)
trap 'rm -rf "$test_dir"' EXIT
cd "$test_dir"
"$image" --appimage-extract >/dev/null

# Fail even on machines that happen to have the missing libraries installed.
for library in libgtk-4.so.1 libgtksourceview-5.so.0 libfontconfig.so.1 \
    libfreetype.so.6 libharfbuzz.so.0 libfribidi.so.0; do
    if [[ ! -f "squashfs-root/usr/lib/$library" ]]; then
        echo "Missing bundled library: $library" >&2
        exit 1
    fi
done
test -f squashfs-root/usr/share/gtksourceview-5/language-specs/rust.lang
test -f squashfs-root/usr/share/gtksourceview-5/styles/Adwaita.xml
test -f squashfs-root/usr/share/glib-2.0/schemas/gschemas.compiled
test -f squashfs-root/etc/fonts/fonts.conf
test -f squashfs-root/usr/share/mime/mime.cache

# Match the build's glibc baseline, without installing GTK or GtkSourceView.
# Extracting first avoids requiring FUSE or privileged containers.
docker run --rm --volume "$test_dir/squashfs-root:/app:ro" \
    --volume "$runtime_test:/smoke-test.sh:ro" ubuntu:24.04 bash -euc '
    apt-get update -qq
    apt-get install -y --no-install-recommends xvfb xauth xdotool dbus-x11 \
        fonts-dejavu-core libgl1 libegl1 libgl1-mesa-dri >/dev/null
    bash /smoke-test.sh
'
