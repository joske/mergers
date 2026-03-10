#[allow(clippy::wildcard_imports)]
use super::*;

pub fn setup_diff_tags(buffer: &TextBuffer) {
    let table = buffer.tag_table();
    // Search match highlighting
    table.add(
        &TextTag::builder()
            .name("search-match")
            .background(search_match_bg())
            .build(),
    );
    table.add(
        &TextTag::builder()
            .name("search-current")
            .background(search_current_bg())
            .build(),
    );
    // Inline word-level highlighting (meld-style colors)
    table.add(
        &TextTag::builder()
            .name("inline-changed")
            .background(inline_changed())
            .build(),
    );
    table.add(
        &TextTag::builder()
            .name("inline-deleted")
            .background(inline_deleted())
            .build(),
    );
    table.add(
        &TextTag::builder()
            .name("inline-inserted")
            .background(inline_inserted())
            .build(),
    );
    // Chunk background fills (rendered behind text via paragraph_background).
    // No foreground override — syntax highlighting is preserved as-is,
    // matching Meld's approach (background-only, no text colour change).
    for (name, bg) in [
        ("chunk-insert-bg", chunk_bg_insert()),
        ("chunk-replace-bg", chunk_bg_replace()),
        ("chunk-conflict-bg", chunk_bg_conflict()),
    ] {
        table.add(
            &TextTag::builder()
                .name(name)
                .paragraph_background(bg)
                .build(),
        );
    }
}

/// Update existing diff tag colours to match the current light/dark scheme.
pub fn update_diff_tag_colors(buf: &TextBuffer) {
    let table = buf.tag_table();
    let bg_updates: &[(&str, &str)] = &[
        ("search-match", search_match_bg()),
        ("search-current", search_current_bg()),
        ("inline-changed", inline_changed()),
        ("inline-deleted", inline_deleted()),
        ("inline-inserted", inline_inserted()),
    ];
    for &(name, bg) in bg_updates {
        if let Some(tag) = table.lookup(name) {
            tag.set_background(Some(bg));
        }
    }
    for (name, bg) in [
        ("chunk-insert-bg", chunk_bg_insert()),
        ("chunk-replace-bg", chunk_bg_replace()),
        ("chunk-conflict-bg", chunk_bg_conflict()),
    ] {
        if let Some(tag) = table.lookup(name) {
            tag.set_paragraph_background(Some(bg));
        }
    }
}

/// Register source-mark attributes for conflict-region line backgrounds.
///
/// `GtkSourceView` renders mark backgrounds below text on **all** lines
/// (including empty ones), unlike `paragraph_background` tags which skip
/// empty lines in some GTK4 versions.
pub fn setup_conflict_marks(sv: &sourceview5::View) {
    let bg_hex = chunk_bg_conflict();
    if let Ok(rgba) = gtk4::gdk::RGBA::parse(bg_hex) {
        let attrs = sourceview5::MarkAttributes::new();
        attrs.set_background(&rgba);
        sv.set_mark_attributes("conflict-bg", &attrs, 1);
    }
}

thread_local! {
    static SCHEME_PROVIDER: CssProvider = CssProvider::new();
    static SCHEME_REGISTERED: Cell<bool> = const { Cell::new(false) };
    pub static IS_DARK_SCHEME: Cell<bool> = const { Cell::new(false) };
}

/// Whether the current sourceview scheme has a dark background.
pub fn is_dark_scheme() -> bool {
    IS_DARK_SCHEME.with(Cell::get)
}

/// Compute perceived luminance from a CSS hex colour string (e.g. `"#2e3436"`).
#[must_use]
pub fn hex_luminance(hex: &str) -> f64 {
    let hex = hex.trim_start_matches('#');
    if hex.len() < 6 {
        return 1.0; // assume light if unparseable
    }
    let r = f64::from(u8::from_str_radix(&hex[0..2], 16).unwrap_or(255)) / 255.0;
    let g = f64::from(u8::from_str_radix(&hex[2..4], 16).unwrap_or(255)) / 255.0;
    let b = f64::from(u8::from_str_radix(&hex[4..6], 16).unwrap_or(255)) / 255.0;
    0.299 * r + 0.587 * g + 0.114 * b
}

/// Apply the sourceview scheme's text background/foreground via a global CSS
/// provider targeting `.meld-editor`, overriding any GTK dark theme interference.
pub fn apply_scheme_css(settings: &Settings) {
    let scheme_mgr = sourceview5::StyleSchemeManager::default();
    let Some(scheme) = scheme_mgr.scheme(&settings.style_scheme) else {
        return;
    };
    let style = scheme.style("text");
    let bg = style.as_ref().and_then(sourceview5::Style::background);
    let fg = style.as_ref().and_then(sourceview5::Style::foreground);

    // Determine dark/light from scheme background luminance.
    // If the scheme doesn't define a text background, fall back to GTK's
    // dark-mode preference (like Meld does with theme_bg_color).
    let dark = bg.as_ref().map_or_else(
        || gtk4::Settings::default().is_some_and(|s| super::detect_dark_mode(&s)),
        |hex| hex_luminance(hex) < 0.5,
    );
    IS_DARK_SCHEME.with(|c| c.set(dark));

    let mut rules = Vec::new();
    if let Some(ref bg) = bg {
        rules.push(format!("background-color: {bg};"));
    }
    if let Some(ref fg) = fg {
        rules.push(format!("color: {fg};"));
    }
    if rules.is_empty() {
        return;
    }
    let css = format!(".meld-editor text {{ {} }}", rules.join(" "));
    SCHEME_PROVIDER.with(|provider| {
        provider.load_from_string(&css);
        SCHEME_REGISTERED.with(|reg| {
            if !reg.get() {
                gtk4::style_context_add_provider_for_display(
                    &Display::default().expect("GTK display must be available"),
                    provider,
                    gtk4::STYLE_PROVIDER_PRIORITY_USER,
                );
                reg.set(true);
            }
        });
    });
}

#[must_use]
pub fn create_source_buffer(file_path: &Path, settings: &Settings) -> TextBuffer {
    let buf = sourceview5::Buffer::new(None::<&gtk4::TextTagTable>);
    let lang_mgr = sourceview5::LanguageManager::default();
    let filename = file_path.file_name().and_then(|n| n.to_str());
    if let Some(lang) = lang_mgr.guess_language(filename, None::<&str>) {
        buf.set_language(Some(&lang));
    }
    buf.set_highlight_syntax(true);
    let scheme_mgr = sourceview5::StyleSchemeManager::default();
    if let Some(scheme) = scheme_mgr.scheme(&settings.style_scheme) {
        buf.set_style_scheme(Some(&scheme));
    }
    setup_diff_tags(buf.upcast_ref());
    buf.upcast()
}

pub fn remove_diff_tags(buf: &TextBuffer) {
    let start = buf.start_iter();
    let end = buf.end_iter();
    for name in &[
        "inline-changed",
        "inline-deleted",
        "inline-inserted",
        "chunk-insert-bg",
        "chunk-replace-bg",
        "chunk-conflict-bg",
    ] {
        if let Some(tag) = buf.tag_table().lookup(name) {
            buf.remove_tag(&tag, &start, &end);
        }
    }
}

// ─── Theme-aware colour helpers ─────────────────────────────────────────────
//
// Each function returns the appropriate colour for the current scheme (light or
// dark), determined by the `IS_DARK_SCHEME` thread-local set in `apply_scheme_css`.
//
// Light colours are taken from Meld's meld-base.style-scheme.xml;
// dark colours from meld-dark.style-scheme.xml.
// Chunk fills use opaque `paragraph_background` TextTags (rendered behind text).
// Strokes, bands, and fillers use Cairo with alpha (drawn on an overlay).

#[must_use]
pub fn stroke_insert() -> (f64, f64, f64, f64) {
    if is_dark_scheme() {
        (0.30, 0.65, 0.18, 0.8) // bright green
    } else {
        (0.647, 1.0, 0.298, 0.7) // #a5ff4c
    }
}
#[must_use]
pub fn stroke_replace() -> (f64, f64, f64, f64) {
    if is_dark_scheme() {
        (0.20, 0.50, 0.90, 0.8) // bright blue
    } else {
        (0.396, 0.698, 1.0, 0.7) // #65b2ff
    }
}
#[must_use]
pub fn stroke_conflict() -> (f64, f64, f64, f64) {
    if is_dark_scheme() {
        (0.90, 0.35, 0.30, 0.8) // bright red
    } else {
        (1.0, 0.31, 0.298, 0.7) // #ff4f4c
    }
}

#[must_use]
pub fn band_insert() -> (f64, f64, f64) {
    if is_dark_scheme() {
        (0.30, 0.65, 0.18) // bright green
    } else {
        (0.647, 1.0, 0.298) // #a5ff4c
    }
}
#[must_use]
pub fn band_replace() -> (f64, f64, f64) {
    if is_dark_scheme() {
        (0.20, 0.50, 0.90) // bright blue
    } else {
        (0.396, 0.698, 1.0) // #65b2ff
    }
}

#[must_use]
pub fn band_conflict() -> (f64, f64, f64) {
    if is_dark_scheme() {
        (0.90, 0.35, 0.30) // bright red
    } else {
        (1.0, 0.31, 0.298) // #ff4f4c
    }
}

#[must_use]
pub fn filler_insert() -> (f64, f64, f64) {
    band_insert()
}
#[must_use]
pub fn filler_replace() -> (f64, f64, f64) {
    band_replace()
}
#[must_use]
pub fn inline_changed() -> &'static str {
    if is_dark_scheme() {
        "#8a8a3c" // visible yellow
    } else {
        "#c8c864"
    }
}
#[must_use]
pub fn inline_deleted() -> &'static str {
    if is_dark_scheme() {
        "#8a3c3c" // visible red
    } else {
        "#ff9696"
    }
}
#[must_use]
pub fn inline_inserted() -> &'static str {
    if is_dark_scheme() {
        "#3c8a3c" // visible green
    } else {
        "#64c864"
    }
}

#[must_use]
pub fn search_match_bg() -> &'static str {
    if is_dark_scheme() {
        "#9a7a10"
    } else {
        "#ffe066"
    }
}
#[must_use]
pub fn search_current_bg() -> &'static str {
    if is_dark_scheme() {
        "#b86010"
    } else {
        "#ff9632"
    }
}

// Opaque chunk background colours used by `paragraph_background` TextTags
// (drawn behind text by the text view, matching Meld's rendering approach).
#[must_use]
pub fn chunk_bg_insert() -> &'static str {
    if is_dark_scheme() {
        "#1e4e0e" // visible dark green
    } else {
        "#b0e080" // saturated green (readable with muted-text schemes)
    }
}
#[must_use]
pub fn chunk_bg_replace() -> &'static str {
    if is_dark_scheme() {
        "#0e2e6e" // visible dark blue
    } else {
        "#a0c8ee" // saturated blue
    }
}
#[must_use]
pub fn chunk_bg_conflict() -> &'static str {
    if is_dark_scheme() {
        "#6e2420" // visible dark red
    } else {
        "#f09090" // saturated red
    }
}

/// Apply `paragraph_background` tags for chunk fills (drawn behind text).
pub fn apply_chunk_bg_tags(buf: &TextBuffer, chunks: &[DiffChunk], side: Side) {
    let start = buf.start_iter();
    let end = buf.end_iter();
    for name in ["chunk-insert-bg", "chunk-replace-bg"] {
        if let Some(tag) = buf.tag_table().lookup(name) {
            buf.remove_tag(&tag, &start, &end);
        }
    }
    for chunk in chunks {
        if chunk.tag == DiffTag::Equal {
            continue;
        }
        let (line_start, line_end) = match side {
            Side::A => (chunk.start_a, chunk.end_a),
            Side::B => (chunk.start_b, chunk.end_b),
        };
        if line_start == line_end {
            continue;
        }
        let other_has_content = match side {
            Side::A => chunk.end_b > chunk.start_b,
            Side::B => chunk.end_a > chunk.start_a,
        };
        let tag_name = if other_has_content {
            "chunk-replace-bg"
        } else {
            "chunk-insert-bg"
        };
        let s = buf
            .iter_at_line(line_start as i32)
            .unwrap_or_else(|| buf.end_iter());
        let e = if line_end as i32 >= buf.line_count() {
            buf.end_iter()
        } else {
            buf.iter_at_line(line_end as i32)
                .unwrap_or_else(|| buf.end_iter())
        };
        buf.apply_tag_by_name(tag_name, &s, &e);
    }
}

/// Apply `paragraph_background` tags for conflict regions on the merge middle pane.
/// Check whether two line ranges overlap (handling zero-width ranges).
#[must_use]
pub fn chunks_overlap(a_start: usize, a_end: usize, b_start: usize, b_end: usize) -> bool {
    if a_start == a_end && b_start == b_end {
        a_start == b_start
    } else if a_start == a_end {
        a_start >= b_start && a_start < b_end
    } else if b_start == b_end {
        b_start >= a_start && b_start < a_end
    } else {
        let overlap_start = a_start.max(b_start);
        let overlap_end = a_end.max(a_start).min(b_end.max(b_start));
        overlap_start < overlap_end
    }
}

/// Apply `paragraph_background` conflict tags on the middle pane of a 3-way merge.
pub fn apply_conflict_bg_tags(
    buf: &TextBuffer,
    left_chunks: &[DiffChunk],
    right_chunks: &[DiffChunk],
) {
    let sv_buf: &sourceview5::Buffer = buf
        .downcast_ref()
        .expect("buffer must be a sourceview5::Buffer");
    let start = buf.start_iter();
    let end = buf.end_iter();
    sv_buf.remove_source_marks(&start, &end, Some("conflict-bg"));
    if let Some(tag) = buf.tag_table().lookup("chunk-conflict-bg") {
        buf.remove_tag(&tag, &start, &end);
    }
    for lc in left_chunks {
        if lc.tag == DiffTag::Equal {
            continue;
        }
        for rc in right_chunks {
            if rc.tag == DiffTag::Equal {
                continue;
            }
            if !chunks_overlap(lc.start_b, lc.end_b, rc.start_a, rc.end_a) {
                continue;
            }
            let tag_start = lc.start_b.min(rc.start_a);
            let tag_end = lc.end_b.max(rc.end_a);
            if tag_start == tag_end {
                continue;
            }
            let s = buf
                .iter_at_line(tag_start as i32)
                .unwrap_or_else(|| buf.end_iter());
            let e = if tag_end as i32 >= buf.line_count() {
                buf.end_iter()
            } else {
                buf.iter_at_line(tag_end as i32)
                    .unwrap_or_else(|| buf.end_iter())
            };
            // Remove insert/replace tags so conflict mark background shows.
            for name in ["chunk-insert-bg", "chunk-replace-bg"] {
                if let Some(t) = buf.tag_table().lookup(name) {
                    buf.remove_tag(&t, &s, &e);
                }
            }
            buf.apply_tag_by_name("chunk-conflict-bg", &s, &e);
            // Source marks render on empty lines too.
            for line in tag_start..tag_end {
                if let Some(iter) = buf.iter_at_line(line as i32) {
                    sv_buf.create_source_mark(None, "conflict-bg", &iter);
                }
            }
        }
    }
}

/// Apply conflict backgrounds on a side pane of a 3-way merge using source marks.
///
/// Source marks render line backgrounds below text on **all** lines (including
/// empty ones), unlike `paragraph_background` tags which skip empty lines.
/// Call *after* `apply_chunk_bg_tags` so insert/replace are already present —
/// this function removes those tags from conflict regions so the mark background
/// shows through.
pub fn apply_side_conflict_bg_tags(
    buf: &TextBuffer,
    left_chunks: &[DiffChunk],
    right_chunks: &[DiffChunk],
    side: Side,
) {
    let sv_buf: &sourceview5::Buffer = buf
        .downcast_ref()
        .expect("buffer must be a sourceview5::Buffer");

    // Remove existing conflict marks and paragraph_background conflict tags.
    let start = buf.start_iter();
    let end = buf.end_iter();
    sv_buf.remove_source_marks(&start, &end, Some("conflict-bg"));
    if let Some(tag) = buf.tag_table().lookup("chunk-conflict-bg") {
        buf.remove_tag(&tag, &start, &end);
    }

    let my_chunks: &[DiffChunk] = match side {
        Side::A => left_chunks,
        Side::B => right_chunks,
    };
    let other_chunks: &[DiffChunk] = match side {
        Side::A => right_chunks,
        Side::B => left_chunks,
    };

    let merged = merged_gutter_chunks(my_chunks, other_chunks, side);
    for (chunk, is_conflict) in &merged {
        if !is_conflict {
            continue;
        }
        let (tag_start, tag_end) = match side {
            Side::A => (chunk.start_a, chunk.end_a),
            Side::B => (chunk.start_b, chunk.end_b),
        };
        if tag_start >= tag_end {
            continue;
        }
        let s = buf
            .iter_at_line(tag_start as i32)
            .unwrap_or_else(|| buf.end_iter());
        let e = if tag_end as i32 >= buf.line_count() {
            buf.end_iter()
        } else {
            buf.iter_at_line(tag_end as i32)
                .unwrap_or_else(|| buf.end_iter())
        };
        // Apply conflict paragraph_background (overrides any insert/replace tags
        // by GTK tag priority) and source marks (for empty lines where
        // paragraph_background doesn't render).
        for name in ["chunk-insert-bg", "chunk-replace-bg"] {
            if let Some(t) = buf.tag_table().lookup(name) {
                buf.remove_tag(&t, &s, &e);
            }
        }
        buf.apply_tag_by_name("chunk-conflict-bg", &s, &e);
        for line in tag_start..tag_end {
            if let Some(iter) = buf.iter_at_line(line as i32) {
                sv_buf.create_source_mark(None, "conflict-bg", &iter);
            }
        }
    }
}

/// Draw chunk stroke lines via Cairo overlay (fills are handled by `paragraph_background` tags).
///
/// When `is_conflict` is `Some`, chunks where `is_conflict[i]` is true are
/// skipped — the conflict drawing functions handle those separately.
#[allow(clippy::too_many_arguments)]
pub fn draw_chunk_backgrounds(
    cr: &gtk4::cairo::Context,
    width: f64,
    tv: &TextView,
    scroll: &ScrolledWindow,
    chunks: &[DiffChunk],
    side: Side,
    current_chunk_idx: Option<usize>,
    is_conflict: Option<&[bool]>,
) {
    let scroll_y = scroll.vadjustment().value();
    let view_h = scroll.vadjustment().page_size();
    let buf = tv.buffer();

    for (i, chunk) in chunks.iter().enumerate() {
        if chunk.tag == DiffTag::Equal {
            continue;
        }

        let (start, end) = match side {
            Side::A => (chunk.start_a, chunk.end_a),
            Side::B => (chunk.start_b, chunk.end_b),
        };

        // Skip empty ranges (e.g. pure insert on the other side)
        if start == end {
            continue;
        }

        // Skip conflict chunks — drawn by conflict functions instead.
        if is_conflict.is_some_and(|c| c[i]) {
            continue;
        }

        // Compute Y start (top of first line)
        let y_top_buf = if let Some(iter) = buf.iter_at_line(start as i32) {
            tv.line_yrange(&iter).0 as f64
        } else {
            let iter = buf.end_iter();
            let (y, h) = tv.line_yrange(&iter);
            (y + h) as f64
        };

        // Compute Y end (bottom of last line)
        let last = end - 1;
        let y_bot_buf = if let Some(iter) = buf.iter_at_line(last as i32) {
            let (y, h) = tv.line_yrange(&iter);
            (y + h) as f64
        } else {
            let iter = buf.end_iter();
            let (y, h) = tv.line_yrange(&iter);
            (y + h) as f64
        };

        let y_top = y_top_buf - scroll_y;
        let y_bot = y_bot_buf - scroll_y;

        // Skip if not visible
        if y_bot < 0.0 || y_top > view_h {
            continue;
        }

        // Stroke color based on whether the OTHER side also has content
        let other_has_content = match side {
            Side::A => chunk.end_b > chunk.start_b,
            Side::B => chunk.end_a > chunk.start_a,
        };
        let stroke = if other_has_content {
            stroke_replace()
        } else {
            stroke_insert()
        };

        // Outline strokes (top and bottom) — fill is handled by paragraph_background tags
        if current_chunk_idx == Some(i) {
            // Current chunk: bold borders (darken on light, brighten on dark)
            let bold = if is_dark_scheme() {
                (
                    (stroke.0 * 0.5 + 0.5).min(1.0),
                    (stroke.1 * 0.5 + 0.5).min(1.0),
                    (stroke.2 * 0.5 + 0.5).min(1.0),
                )
            } else {
                (stroke.0 * 0.5, stroke.1 * 0.5, stroke.2 * 0.5)
            };
            cr.set_source_rgba(bold.0, bold.1, bold.2, 1.0);
            cr.rectangle(0.0, y_top - 2.0, width, 3.0);
            let _ = cr.fill();
            cr.rectangle(0.0, y_bot - 1.0, width, 3.0);
            let _ = cr.fill();
        } else {
            cr.set_source_rgba(stroke.0, stroke.1, stroke.2, stroke.3);
            cr.rectangle(0.0, y_top, width, 1.0);
            let _ = cr.fill();
            cr.rectangle(0.0, y_bot - 1.0, width, 1.0);
            let _ = cr.fill();
        }
    }
}

/// Compute per-chunk conflict flags: `result[i]` is true when chunk `i` in
/// `my_chunks` overlaps with any non-Equal chunk from `other_chunks` on the
/// middle pane.
///
/// `my_mid` selects which side of `my_chunks` is the middle pane;
/// `other_mid` selects which side of `other_chunks` is the middle pane.
#[must_use]
pub fn conflict_flags(
    my_chunks: &[DiffChunk],
    my_mid: Side,
    other_chunks: &[DiffChunk],
    other_mid: Side,
) -> Vec<bool> {
    // Pre-collect non-Equal other-chunks with their mid-pane ranges.
    // These are already sorted by position since chunks are in order.
    let others: Vec<(usize, usize)> = other_chunks
        .iter()
        .filter(|oc| oc.tag != DiffTag::Equal)
        .map(|oc| match other_mid {
            Side::A => (oc.start_a, oc.end_a),
            Side::B => (oc.start_b, oc.end_b),
        })
        .collect();

    // Two-pointer sweep: j only advances forward across all my_chunks → O(n+m).
    let mut j = 0;
    my_chunks
        .iter()
        .map(|mc| {
            if mc.tag == DiffTag::Equal {
                return false;
            }
            let (ms, me) = match my_mid {
                Side::A => (mc.start_a, mc.end_a),
                Side::B => (mc.start_b, mc.end_b),
            };
            // Advance j past other-chunks that end entirely before this chunk starts.
            // For a normal range (os < oe): it's fully before when oe <= ms.
            // For a zero-width point range (os == oe): it's fully before when os < ms.
            while j < others.len() {
                let (os, oe) = others[j];
                if os == oe {
                    if os < ms {
                        j += 1;
                    } else {
                        break;
                    }
                } else if oe <= ms {
                    j += 1;
                } else {
                    break;
                }
            }
            // Check all other-chunks starting from j that could still overlap.
            // Once an other-chunk starts at or beyond our end (with no overlap),
            // no further chunks can overlap either.
            for &(os, oe) in &others[j..] {
                if chunks_overlap(ms, me, os, oe) {
                    return true;
                }
                // If this other-chunk starts at or beyond our end, later ones
                // are even further away, so stop.
                if ms == me {
                    // Point range: no overlap possible once os > ms.
                    if os > ms {
                        break;
                    }
                } else {
                    // Normal range: no overlap once os >= me.
                    if os >= me {
                        break;
                    }
                }
            }
            false
        })
        .collect()
}

/// Draw conflict background overlays on the middle pane of a 3-way merge.
///
/// Uses merged conflict regions so overlapping left/right pairs produce a
/// single set of borders instead of duplicates.
///
/// `current_chunk` is `(chunk_index, is_right)` — the currently selected chunk.
/// When a conflict involves the current chunk, it gets bold borders.
#[allow(clippy::too_many_arguments)]
pub fn draw_conflict_backgrounds(
    cr: &gtk4::cairo::Context,
    width: f64,
    tv: &TextView,
    scroll: &ScrolledWindow,
    left_chunks: &[DiffChunk],
    right_chunks: &[DiffChunk],
    current_chunk: Option<(usize, bool)>,
) {
    let scroll_y = scroll.vadjustment().value();
    let view_h = scroll.vadjustment().page_size();
    let buf = tv.buffer();

    let regions = middle_conflict_regions(left_chunks, right_chunks);

    for &(tag_start, tag_end) in &regions {
        if tag_start == tag_end {
            continue;
        }

        let y_top_buf = if let Some(iter) = buf.iter_at_line(tag_start as i32) {
            tv.line_yrange(&iter).0 as f64
        } else {
            let iter = buf.end_iter();
            let (y, h) = tv.line_yrange(&iter);
            (y + h) as f64
        };

        let last = tag_end - 1;
        let y_bot_buf = if let Some(iter) = buf.iter_at_line(last as i32) {
            let (y, h) = tv.line_yrange(&iter);
            (y + h) as f64
        } else {
            let iter = buf.end_iter();
            let (y, h) = tv.line_yrange(&iter);
            (y + h) as f64
        };

        let y_top = y_top_buf - scroll_y;
        let y_bot = y_bot_buf - scroll_y;

        if y_bot < 0.0 || y_top > view_h {
            continue;
        }

        // A conflict region is "current" if the current chunk (from either
        // side) has its middle-pane projection overlapping this region.
        let is_current = current_chunk.is_some_and(|(idx, is_right)| {
            if is_right {
                let c = &right_chunks[idx];
                chunks_overlap(c.start_a, c.end_a, tag_start, tag_end)
            } else {
                let c = &left_chunks[idx];
                chunks_overlap(c.start_b, c.end_b, tag_start, tag_end)
            }
        });

        let cs = stroke_conflict();
        if is_current {
            let bold = if is_dark_scheme() {
                (
                    (cs.0 * 0.5 + 0.5).min(1.0),
                    (cs.1 * 0.5 + 0.5).min(1.0),
                    (cs.2 * 0.5 + 0.5).min(1.0),
                )
            } else {
                (cs.0 * 0.5, cs.1 * 0.5, cs.2 * 0.5)
            };
            cr.set_source_rgba(bold.0, bold.1, bold.2, 1.0);
            cr.rectangle(0.0, y_top - 2.0, width, 3.0);
            let _ = cr.fill();
            cr.rectangle(0.0, y_bot - 1.0, width, 3.0);
            let _ = cr.fill();
        } else {
            cr.set_source_rgba(cs.0, cs.1, cs.2, 1.0);
            cr.rectangle(0.0, y_top, width, 1.0);
            let _ = cr.fill();
            cr.rectangle(0.0, y_bot - 1.0, width, 1.0);
            let _ = cr.fill();
        }
    }
}

/// Draw conflict stroke lines on a side pane (left or right) of a 3-way merge.
///
/// Uses `merged_gutter_chunks` so strokes match the fills from
/// `apply_side_conflict_bg_tags`.
///
/// `current_chunk` is `(chunk_index, is_right)`. On the left pane (side=A)
/// the current chunk uses `is_right=false`; on the right pane (side=B) `is_right=true`.
#[allow(clippy::too_many_arguments)]
pub fn draw_side_conflict_strokes(
    cr: &gtk4::cairo::Context,
    width: f64,
    tv: &TextView,
    scroll: &ScrolledWindow,
    left_chunks: &[DiffChunk],
    right_chunks: &[DiffChunk],
    side: Side,
    current_chunk: Option<(usize, bool)>,
) {
    let scroll_y = scroll.vadjustment().value();
    let view_h = scroll.vadjustment().page_size();
    let buf = tv.buffer();
    let my_chunks: &[DiffChunk] = match side {
        Side::A => left_chunks,
        Side::B => right_chunks,
    };
    let other_chunks: &[DiffChunk] = match side {
        Side::A => right_chunks,
        Side::B => left_chunks,
    };

    let merged = merged_gutter_chunks(my_chunks, other_chunks, side);
    for (chunk, is_conflict) in &merged {
        if !is_conflict {
            continue;
        }
        let (start, end) = match side {
            Side::A => (chunk.start_a, chunk.end_a),
            Side::B => (chunk.start_b, chunk.end_b),
        };
        if start >= end {
            continue;
        }

        // Check if the current chunk is involved in this conflict band.
        // Use middle-pane overlap: project both the current chunk and this
        // band onto the middle pane and check for overlap.
        let (band_mid_s, band_mid_e) = match side {
            Side::A => (chunk.start_b, chunk.end_b),
            Side::B => (chunk.start_a, chunk.end_a),
        };
        let is_current = current_chunk.is_some_and(|(idx, is_right)| {
            let (cur_mid_s, cur_mid_e) = if is_right {
                let c = &right_chunks[idx];
                (c.start_a, c.end_a) // right_chunks: A=middle
            } else {
                let c = &left_chunks[idx];
                (c.start_b, c.end_b) // left_chunks: B=middle
            };
            chunks_overlap(cur_mid_s, cur_mid_e, band_mid_s, band_mid_e)
        });

        let y_top_buf = if let Some(iter) = buf.iter_at_line(start as i32) {
            tv.line_yrange(&iter).0 as f64
        } else {
            let iter = buf.end_iter();
            let (y, h) = tv.line_yrange(&iter);
            (y + h) as f64
        };
        let last = end - 1;
        let y_bot_buf = if let Some(iter) = buf.iter_at_line(last as i32) {
            let (y, h) = tv.line_yrange(&iter);
            (y + h) as f64
        } else {
            let iter = buf.end_iter();
            let (y, h) = tv.line_yrange(&iter);
            (y + h) as f64
        };
        let y_top = y_top_buf - scroll_y;
        let y_bot = y_bot_buf - scroll_y;
        if y_bot >= 0.0 && y_top <= view_h {
            let cs = stroke_conflict();
            if is_current {
                let bold = if is_dark_scheme() {
                    (
                        (cs.0 * 0.5 + 0.5).min(1.0),
                        (cs.1 * 0.5 + 0.5).min(1.0),
                        (cs.2 * 0.5 + 0.5).min(1.0),
                    )
                } else {
                    (cs.0 * 0.5, cs.1 * 0.5, cs.2 * 0.5)
                };
                cr.set_source_rgba(bold.0, bold.1, bold.2, 1.0);
                cr.rectangle(0.0, y_top - 2.0, width, 3.0);
                let _ = cr.fill();
                cr.rectangle(0.0, y_bot - 1.0, width, 3.0);
                let _ = cr.fill();
            } else {
                cr.set_source_rgba(cs.0, cs.1, cs.2, 1.0);
                cr.rectangle(0.0, y_top, width, 1.0);
                let _ = cr.fill();
                cr.rectangle(0.0, y_bot - 1.0, width, 1.0);
                let _ = cr.fill();
            }
        }
    }
}

/// Draw thin colored lines where the other side has content that this side lacks.
pub fn draw_fillers(
    cr: &gtk4::cairo::Context,
    width: f64,
    tv: &TextView,
    scroll: &ScrolledWindow,
    chunks: &[DiffChunk],
    side_is_left: bool,
) {
    let scroll_y = scroll.vadjustment().value();
    let view_h = scroll.vadjustment().page_size();
    let line_thickness = 2.0_f64;

    for chunk in chunks {
        if chunk.tag == DiffTag::Equal {
            continue;
        }
        let left_lines = chunk.end_a - chunk.start_a;
        let right_lines = chunk.end_b - chunk.start_b;
        if left_lines == right_lines {
            continue;
        }

        // Determine if this pane needs a filler indicator for this chunk
        let (need_filler, anchor) = if side_is_left && right_lines > left_lines {
            (true, chunk.end_a)
        } else if !side_is_left && left_lines > right_lines {
            (true, chunk.end_b)
        } else {
            (false, 0)
        };
        if !need_filler {
            continue;
        }

        let color = match chunk.tag {
            DiffTag::Delete | DiffTag::Insert => filler_insert(),
            DiffTag::Replace => filler_replace(),
            DiffTag::Equal => unreachable!(),
        };

        let buf = tv.buffer();
        let anchor_y_buf = {
            let anchor_i32 = anchor.min(buf.line_count().max(0) as usize) as i32;
            if let Some(iter) = buf.iter_at_line(anchor_i32) {
                tv.line_yrange(&iter).0 as f64
            } else {
                let iter = buf.end_iter();
                let (y, h) = tv.line_yrange(&iter);
                (y + h) as f64
            }
        };

        let line_y = anchor_y_buf - scroll_y;

        // Skip if not visible
        if line_y + line_thickness < 0.0 || line_y - line_thickness > view_h {
            continue;
        }

        cr.set_source_rgb(color.0, color.1, color.2);
        cr.rectangle(0.0, line_y - line_thickness / 2.0, width, line_thickness);
        let _ = cr.fill();
    }
}

/// Like `draw_fillers` but uses conflict colour for chunks whose middle-pane
/// range overlaps with the other diff.
#[allow(clippy::too_many_arguments)]
pub fn draw_merge_fillers(
    cr: &gtk4::cairo::Context,
    width: f64,
    tv: &TextView,
    scroll: &ScrolledWindow,
    chunks: &[DiffChunk],
    side_is_left: bool,
    left_chunks: &[DiffChunk],
    right_chunks: &[DiffChunk],
) {
    let scroll_y = scroll.vadjustment().value();
    let view_h = scroll.vadjustment().page_size();
    let line_thickness = 2.0_f64;

    // Compute side-pane conflict bands so we can check filler anchors.
    let (my_ch, oth_ch) = if side_is_left {
        (left_chunks, right_chunks)
    } else {
        (right_chunks, left_chunks)
    };
    let pane_side = if side_is_left { Side::A } else { Side::B };
    let gutter_merged = merged_gutter_chunks(my_ch, oth_ch, pane_side);
    let side_bands: Vec<(usize, usize)> = gutter_merged
        .iter()
        .filter(|(_, is_c)| *is_c)
        .map(|(ch, _)| match pane_side {
            Side::A => (ch.start_a, ch.end_a),
            Side::B => (ch.start_b, ch.end_b),
        })
        .collect();

    for chunk in chunks {
        if chunk.tag == DiffTag::Equal {
            continue;
        }
        let left_lines = chunk.end_a - chunk.start_a;
        let right_lines = chunk.end_b - chunk.start_b;
        if left_lines == right_lines {
            continue;
        }

        let (need_filler, anchor) = if side_is_left && right_lines > left_lines {
            (true, chunk.end_a)
        } else if !side_is_left && left_lines > right_lines {
            (true, chunk.end_b)
        } else {
            (false, 0)
        };
        if !need_filler {
            continue;
        }

        // Skip fillers whose anchor falls within a conflict band — the conflict
        // background already covers the region, so a filler line is just noise.
        if side_bands.iter().any(|&(s, e)| anchor >= s && anchor <= e) {
            continue;
        }

        let color = match chunk.tag {
            DiffTag::Delete | DiffTag::Insert => filler_insert(),
            DiffTag::Replace => filler_replace(),
            DiffTag::Equal => unreachable!(),
        };

        let buf = tv.buffer();
        let anchor_y_buf = {
            let anchor_i32 = anchor.min(buf.line_count().max(0) as usize) as i32;
            if let Some(iter) = buf.iter_at_line(anchor_i32) {
                tv.line_yrange(&iter).0 as f64
            } else {
                let iter = buf.end_iter();
                let (y, h) = tv.line_yrange(&iter);
                (y + h) as f64
            }
        };

        let line_y = anchor_y_buf - scroll_y;
        if line_y + line_thickness < 0.0 || line_y - line_thickness > view_h {
            continue;
        }

        cr.set_source_rgb(color.0, color.1, color.2);
        cr.rectangle(0.0, line_y - line_thickness / 2.0, width, line_thickness);
        let _ = cr.fill();
    }
}

#[must_use]
pub fn get_line_text(buf: &TextBuffer, line: usize) -> String {
    let start = buf
        .iter_at_line(line as i32)
        .unwrap_or_else(|| buf.start_iter());
    let mut end = start;
    end.forward_to_line_end();
    buf.text(&start, &end, false).to_string()
}

pub fn apply_char_tag(
    buf: &TextBuffer,
    tag_name: &str,
    line: usize,
    tokens: &[myers::Token<'_>],
    tok_start: usize,
    tok_end: usize,
) {
    if tok_start >= tok_end || tok_start >= tokens.len() {
        return;
    }
    let byte_start = tokens[tok_start].offset;
    let byte_end = if tok_end < tokens.len() {
        tokens[tok_end].offset
    } else {
        tokens.last().map_or(0, |t| t.offset + t.text.len())
    };
    // iter_at_line_offset uses character (not byte) offsets, so convert.
    // Use .get() to avoid panics if offsets fall mid-UTF-8 or beyond line length
    // (buffer may have changed since async diff computed offsets).
    let line_text = get_line_text(buf, line);
    let Some(start_slice) = line_text.get(..byte_start) else {
        return;
    };
    let Some(end_slice) = line_text.get(..byte_end) else {
        return;
    };
    let char_start = start_slice.chars().count() as i32;
    let char_end = end_slice.chars().count() as i32;
    let s = buf.iter_at_line_offset(line as i32, char_start);
    let e = buf.iter_at_line_offset(line as i32, char_end);
    if let (Some(s), Some(e)) = (s, e) {
        buf.apply_tag_by_name(tag_name, &s, &e);
    }
}

pub fn apply_inline_tags(left_buf: &TextBuffer, right_buf: &TextBuffer, chunk: &DiffChunk) {
    let n = std::cmp::min(chunk.end_a - chunk.start_a, chunk.end_b - chunk.start_b);
    for i in 0..n {
        let left_line = chunk.start_a + i;
        let right_line = chunk.start_b + i;
        let left_text = get_line_text(left_buf, left_line);
        let right_text = get_line_text(right_buf, right_line);

        let (toks_a, toks_b, word_chunks) = myers::diff_words(&left_text, &right_text);

        for wc in &word_chunks {
            match wc.tag {
                DiffTag::Equal => {}
                DiffTag::Replace => {
                    apply_char_tag(
                        left_buf,
                        "inline-changed",
                        left_line,
                        &toks_a,
                        wc.start_a,
                        wc.end_a,
                    );
                    apply_char_tag(
                        right_buf,
                        "inline-changed",
                        right_line,
                        &toks_b,
                        wc.start_b,
                        wc.end_b,
                    );
                }
                DiffTag::Delete => {
                    apply_char_tag(
                        left_buf,
                        "inline-deleted",
                        left_line,
                        &toks_a,
                        wc.start_a,
                        wc.end_a,
                    );
                }
                DiffTag::Insert => {
                    apply_char_tag(
                        right_buf,
                        "inline-inserted",
                        right_line,
                        &toks_b,
                        wc.start_b,
                        wc.end_b,
                    );
                }
            }
        }
    }
}

pub struct DiffPane {
    pub container: GtkBox,
    pub text_view: TextView,
    pub scroll: ScrolledWindow,
    pub filler_overlay: DrawingArea,
    pub save_btn: Button,
    pub path_label: Label,
    pub save_path: Rc<RefCell<PathBuf>>,
    pub tab_path: Rc<RefCell<String>>,
}

pub fn make_diff_pane(
    buf: &TextBuffer,
    file_path: &Path,
    info: Option<&str>,
    label_override: Option<&str>,
    settings: &Settings,
) -> DiffPane {
    let sv = sourceview5::View::with_buffer(
        buf.downcast_ref::<sourceview5::Buffer>()
            .expect("buffer must be a sourceview5::Buffer"),
    );
    sv.set_editable(true);
    sv.set_wrap_mode(settings.wrap_mode_gtk());
    sv.set_left_margin(4);
    sv.set_bottom_margin(16);
    sv.set_show_line_numbers(settings.show_line_numbers);
    sv.set_highlight_current_line(settings.highlight_current_line);
    sv.set_tab_width(settings.tab_width);
    sv.set_insert_spaces_instead_of_tabs(settings.insert_spaces);
    if settings.show_whitespace {
        let drawer = sv.space_drawer();
        drawer.set_types_for_locations(
            sourceview5::SpaceLocationFlags::ALL,
            sourceview5::SpaceTypeFlags::ALL,
        );
        drawer.set_enable_matrix(true);
    }
    sv.set_hexpand(true);
    update_font_css(settings);
    apply_scheme_css(settings);
    sv.add_css_class("meld-editor");

    // Register mark attributes for conflict-region backgrounds on merge side
    // panes.  Source marks render backgrounds below text on ALL lines (including
    // empty ones), unlike `paragraph_background` tags.
    setup_conflict_marks(&sv);

    let scroll = ScrolledWindow::builder()
        .min_content_width(360)
        .vexpand(true)
        .child(&sv)
        .build();

    let tv: gtk4::TextView = sv.upcast();

    let header = GtkBox::new(Orientation::Horizontal, 4);
    header.set_margin_start(4);
    header.set_margin_end(4);
    header.set_margin_top(2);
    header.set_margin_bottom(2);
    let save_btn = Button::from_icon_name("document-save-symbolic");
    save_btn.set_tooltip_text(Some("Save"));
    save_btn.set_sensitive(false);
    let display_name = label_override.map_or_else(
        || {
            if is_blank_path(file_path) {
                "Untitled".to_string()
            } else {
                shortened_path(file_path)
            }
        },
        String::from,
    );
    let path_label = Label::new(Some(&display_name));
    let tooltip = if is_blank_path(file_path) {
        "Untitled".to_string()
    } else {
        std::fs::canonicalize(file_path)
            .unwrap_or_else(|_| file_path.to_path_buf())
            .display()
            .to_string()
    };
    path_label.set_tooltip_text(Some(&tooltip));
    path_label.set_ellipsize(gtk4::pango::EllipsizeMode::Start);
    path_label.set_hexpand(true);
    path_label.set_halign(gtk4::Align::Center);
    header.append(&save_btn);
    header.append(&path_label);

    // Enable save button when buffer content changes
    {
        let btn = save_btn.clone();
        buf.connect_changed(move |_| {
            btn.set_sensitive(true);
        });
    }

    let buf_clone = buf.clone();
    let save_path = Rc::new(RefCell::new(file_path.to_path_buf()));
    let tab_path = Rc::new(RefCell::new(file_path.display().to_string()));
    let save_path_clone = save_path.clone();
    let tab_path_clone = tab_path.clone();
    let save_btn_ref = save_btn.clone();
    let path_label_clone = path_label.clone();
    save_btn.connect_clicked(move |_| {
        if is_blank_path(&save_path_clone.borrow()) {
            let dialog = gtk4::FileDialog::builder().title("Save As").build();
            let win = save_btn_ref
                .root()
                .and_then(|r| r.downcast::<ApplicationWindow>().ok());
            let buf = buf_clone.clone();
            let sp = save_path_clone.clone();
            let tp = tab_path_clone.clone();
            let btn = save_btn_ref.clone();
            let lbl = path_label_clone.clone();
            dialog.save(win.as_ref(), gio::Cancellable::NONE, move |result| {
                if let Ok(file) = result
                    && let Some(path) = file.path()
                {
                    let text = buf.text(&buf.start_iter(), &buf.end_iter(), false);
                    match fs::write(&path, text.as_str()) {
                        Ok(()) => {
                            mark_saving(&path);
                            btn.set_sensitive(false);
                            (*sp.borrow_mut()).clone_from(&path);
                            *tp.borrow_mut() = path.display().to_string();
                            lbl.set_text(&shortened_path(&path));
                            lbl.set_tooltip_text(Some(&path.display().to_string()));
                        }
                        Err(e) => {
                            if let Some(win) = btn
                                .root()
                                .and_then(|r| r.downcast::<ApplicationWindow>().ok())
                            {
                                show_error_dialog(
                                    &win,
                                    &format!("Failed to save {}: {e}", path.display()),
                                );
                            }
                        }
                    }
                }
            });
        } else {
            let text = buf_clone.text(&buf_clone.start_iter(), &buf_clone.end_iter(), false);
            save_file(&save_path_clone.borrow(), text.as_str(), &save_btn_ref);
        }
    });

    let filler_overlay = DrawingArea::new();
    filler_overlay.set_can_target(false);

    let overlay = gtk4::Overlay::new();
    overlay.set_child(Some(&scroll));
    overlay.add_overlay(&filler_overlay);

    let vbox = GtkBox::new(Orientation::Vertical, 0);
    vbox.append(&header);
    if let Some(msg) = info {
        vbox.append(&make_info_bar(msg));
    }
    vbox.append(&overlay);

    DiffPane {
        container: vbox,
        text_view: tv,
        scroll,
        filler_overlay,
        save_btn,
        path_label,
        save_path,
        tab_path,
    }
}
