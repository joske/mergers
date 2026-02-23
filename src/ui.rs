#![allow(
    clippy::cast_sign_loss,
    clippy::cast_possible_truncation,
    clippy::cast_possible_wrap,
    clippy::cast_lossless,
    clippy::cast_precision_loss,
    clippy::needless_pass_by_value,
    clippy::similar_names,
    clippy::too_many_lines
)]

use std::{
    cell::{Cell, RefCell},
    collections::{BTreeMap, BTreeSet, HashMap},
    fmt::Write as _,
    fs,
    path::Path,
    rc::Rc,
    sync::mpsc,
    time::{Duration, SystemTime},
};

use chrono::{DateTime, Local};
use gtk4::{
    Adjustment, Application, ApplicationWindow, Box as GtkBox, Button, ColumnView,
    ColumnViewColumn, CssProvider, DrawingArea, Entry, EventControllerFocus, EventControllerKey,
    GestureClick, Image, Label, ListItem, Notebook, Orientation, Paned, PolicyType, PopoverMenu,
    Revealer, ScrolledWindow, SignalListItemFactory, SingleSelection, StringObject, TextBuffer,
    TextSearchFlags, TextTag, TextView, ToggleButton, TreeExpander, TreeListModel, TreeListRow,
    gdk::Display, gio, gio::ListStore, prelude::*,
};
use sourceview5::prelude::*;

use crate::{
    CompareMode,
    myers::{self, DiffChunk, DiffTag},
    settings::Settings,
};

const CSS: &str = r"
.diff-changed { color: #729fcf; font-weight: bold; }
.diff-deleted { color: #f57900; }
.diff-inserted { color: #73d216; }
.diff-missing { color: #888a85; font-style: italic; }
.info-bar { background: #3584e4; padding: 8px 12px; }
.info-bar label { color: white; }
.chunk-label { font-size: 0.9em; }
.linked > button { min-width: 0; padding: 4px 8px; }
.find-bar { background: alpha(@theme_bg_color, 0.95); border-top: 1px solid @borders; padding: 4px 6px; }
.find-bar entry { min-height: 28px; }
.goto-entry { min-height: 28px; }
.dir-pane-focused { border: 2px solid @accent_color; border-radius: 4px; }
";

const SEP: char = '\x1f';

thread_local! {
    static FONT_PROVIDER: CssProvider = CssProvider::new();
    static FONT_REGISTERED: Cell<bool> = const { Cell::new(false) };
}

fn update_font_css(settings: &Settings) {
    let font_desc = gtk4::pango::FontDescription::from_string(&settings.font);
    let css = format!(
        ".meld-editor {{ font-family: \"{}\"; font-size: {}pt; }}",
        font_desc.family().unwrap_or("Monospace".into()),
        font_desc.size() / gtk4::pango::SCALE,
    );
    FONT_PROVIDER.with(|provider| {
        provider.load_from_string(&css);
        FONT_REGISTERED.with(|reg| {
            if !reg.get() {
                gtk4::style_context_add_provider_for_display(
                    &Display::default().unwrap(),
                    provider,
                    gtk4::STYLE_PROVIDER_PRIORITY_APPLICATION + 1,
                );
                reg.set(true);
            }
        });
    });
}

// ─── Data types ────────────────────────────────────────────────────────────

#[derive(Clone, Copy, PartialEq)]
enum FileStatus {
    Same,
    Different,
    LeftOnly,
    RightOnly,
}

impl FileStatus {
    fn code(self) -> &'static str {
        match self {
            Self::Same => "S",
            Self::Different => "D",
            Self::LeftOnly => "L",
            Self::RightOnly => "R",
        }
    }
}

struct DirMeta {
    size: Option<u64>,
    mtime: Option<SystemTime>,
    is_dir: bool,
}

struct FileTab {
    id: u64,
    rel_path: String,
    left_buf: TextBuffer,
    right_buf: TextBuffer,
    left_save: Button,
    right_save: Button,
}

static NEXT_TAB_ID: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
static SAVING: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);

fn apply_diff_tags(left_buf: &TextBuffer, right_buf: &TextBuffer, chunks: &[DiffChunk]) {
    for chunk in chunks {
        match chunk.tag {
            DiffTag::Equal => {}
            DiffTag::Replace => {
                apply_line_tag(left_buf, "changed", chunk.start_a, chunk.end_a);
                apply_line_tag(right_buf, "changed", chunk.start_b, chunk.end_b);
                apply_inline_tags(left_buf, right_buf, chunk);
            }
            DiffTag::Delete => {
                apply_line_tag(left_buf, "deleted", chunk.start_a, chunk.end_a);
            }
            DiffTag::Insert => {
                apply_line_tag(right_buf, "inserted", chunk.start_b, chunk.end_b);
            }
        }
    }
}

/// 3-way merge tagging: outer panes get only their own side's tags,
/// middle pane gets one-sided change highlights + conflict overlay.
fn apply_merge_tags(
    left_buf: &TextBuffer,
    middle_buf: &TextBuffer,
    right_buf: &TextBuffer,
    left_chunks: &[DiffChunk],
    right_chunks: &[DiffChunk],
) {
    // Left pane: a-side of left diff only
    // left_chunks = diff(left, middle): a-side = left, b-side = middle
    for chunk in left_chunks {
        match chunk.tag {
            DiffTag::Equal | DiffTag::Insert => {}
            DiffTag::Replace => {
                apply_line_tag(left_buf, "changed", chunk.start_a, chunk.end_a);
                apply_inline_tags(left_buf, middle_buf, chunk);
            }
            DiffTag::Delete => apply_line_tag(left_buf, "deleted", chunk.start_a, chunk.end_a),
        }
    }

    // Right pane: b-side of right diff only
    // right_chunks = diff(middle, right): a-side = middle, b-side = right
    for chunk in right_chunks {
        match chunk.tag {
            DiffTag::Equal | DiffTag::Delete => {}
            DiffTag::Replace => {
                apply_line_tag(right_buf, "changed", chunk.start_b, chunk.end_b);
                apply_inline_tags(middle_buf, right_buf, chunk);
            }
            DiffTag::Insert => apply_line_tag(right_buf, "inserted", chunk.start_b, chunk.end_b),
        }
    }

    // Middle pane: b-side of left diff (lines in middle that differ from left)
    for chunk in left_chunks {
        match chunk.tag {
            DiffTag::Equal | DiffTag::Delete => {}
            DiffTag::Replace | DiffTag::Insert => {
                apply_line_tag(middle_buf, "changed", chunk.start_b, chunk.end_b);
            }
        }
    }
    // Middle pane: a-side of right diff (lines in middle that differ from right)
    for chunk in right_chunks {
        match chunk.tag {
            DiffTag::Equal | DiffTag::Insert => {}
            DiffTag::Replace | DiffTag::Delete => {
                apply_line_tag(middle_buf, "changed", chunk.start_a, chunk.end_a);
            }
        }
    }

    // Conflict overlay (higher priority tag, added last in setup_diff_tags)
    apply_conflict_tags(middle_buf, left_chunks, right_chunks);
}

fn reload_file_tab(tab: &FileTab, left_dir: &str, right_dir: &str) {
    // Don't overwrite unsaved user edits
    if tab.left_save.is_sensitive() || tab.right_save.is_sensitive() {
        return;
    }

    let left_content =
        fs::read_to_string(Path::new(left_dir).join(&tab.rel_path)).unwrap_or_default();
    let right_content =
        fs::read_to_string(Path::new(right_dir).join(&tab.rel_path)).unwrap_or_default();

    // Only reset buffers if the on-disk content actually differs from what's in the buffer
    let cur_left = tab
        .left_buf
        .text(&tab.left_buf.start_iter(), &tab.left_buf.end_iter(), false);
    let cur_right = tab.right_buf.text(
        &tab.right_buf.start_iter(),
        &tab.right_buf.end_iter(),
        false,
    );

    if cur_left.as_str() == left_content && cur_right.as_str() == right_content {
        return; // nothing changed on disk vs buffer
    }

    // set_text triggers connect_changed which schedules refresh_diff with
    // the current filter state (ignore_blanks / ignore_whitespace).
    // We must NOT run a separate diff here — it would race and potentially
    // overwrite the filtered result with an unfiltered one.
    tab.left_buf.set_text(&left_content);
    tab.right_buf.set_text(&right_content);
    // Buffer now matches disk — clear unsaved-changes indicator.
    // (set_text triggers the save-button connect_changed which sets
    // sensitive=true, so we reset it after.)
    tab.left_save.set_sensitive(false);
    tab.right_save.set_sensitive(false);
}

// ─── Helpers ───────────────────────────────────────────────────────────────

/// Check if a file appears to be binary by looking for NUL bytes in the first 8KB.
fn is_binary(path: &Path) -> bool {
    fs::read(path)
        .map(|bytes| bytes.iter().take(8192).any(|&b| b == 0))
        .unwrap_or(false)
}

/// Read a file as text, returning content and whether it was binary.
/// Binary files return an empty string and `true`.
fn read_file_content(path: &Path) -> (String, bool) {
    if is_binary(path) {
        (String::new(), true)
    } else {
        (fs::read_to_string(path).unwrap_or_default(), false)
    }
}

/// Pre-filter text for diff comparison.
/// Returns `(filtered_text, line_map)` where `line_map[filtered_idx] = original_idx`.
/// - `ignore_whitespace`: collapse each line's whitespace to single spaces.
/// - `ignore_blanks`: remove blank lines (the line map tracks where they were).
fn filter_for_diff(
    text: &str,
    ignore_whitespace: bool,
    ignore_blanks: bool,
) -> (String, Vec<usize>) {
    let lines: Vec<&str> = text.lines().collect();
    let mut filtered = Vec::with_capacity(lines.len());
    let mut line_map = Vec::with_capacity(lines.len());

    for (i, line) in lines.iter().enumerate() {
        if ignore_blanks && line.trim().is_empty() {
            continue;
        }
        if ignore_whitespace {
            filtered.push(line.split_whitespace().collect::<Vec<_>>().join(" "));
        } else {
            filtered.push((*line).to_string());
        }
        line_map.push(i);
    }

    (filtered.join("\n"), line_map)
}

/// Remap diff chunks from filtered line numbers back to original line numbers.
fn remap_chunks(
    chunks: Vec<DiffChunk>,
    left_map: &[usize],
    left_total: usize,
    right_map: &[usize],
    right_total: usize,
) -> Vec<DiffChunk> {
    chunks
        .into_iter()
        .map(|mut chunk| {
            chunk.start_a = left_map.get(chunk.start_a).copied().unwrap_or(left_total);
            chunk.end_a = left_map.get(chunk.end_a).copied().unwrap_or(left_total);
            chunk.start_b = right_map.get(chunk.start_b).copied().unwrap_or(right_total);
            chunk.end_b = right_map.get(chunk.end_b).copied().unwrap_or(right_total);
            chunk
        })
        .collect()
}

fn format_size(bytes: u64) -> String {
    if bytes < 1000 {
        format!("{bytes} B")
    } else if bytes < 1_000_000 {
        format!("{:.1} kB", bytes as f64 / 1000.0)
    } else if bytes < 1_000_000_000 {
        format!("{:.1} MB", bytes as f64 / 1_000_000.0)
    } else {
        format!("{:.1} GB", bytes as f64 / 1_000_000_000.0)
    }
}

fn format_mtime(t: SystemTime) -> String {
    let dt: DateTime<Local> = t.into();
    dt.format("%Y-%m-%d %H:%M:%S").to_string()
}

/// Generate a unified diff (patch) string from chunks and source texts.
fn generate_unified_diff(
    left_label: &str,
    right_label: &str,
    left_text: &str,
    right_text: &str,
    chunks: &[DiffChunk],
) -> String {
    let left_lines: Vec<&str> = left_text.lines().collect();
    let right_lines: Vec<&str> = right_text.lines().collect();
    let mut out = String::new();
    let _ = writeln!(out, "--- {left_label}");
    let _ = writeln!(out, "+++ {right_label}");

    // Group non-Equal chunks into hunks with 3 lines of context
    let context = 3_usize;
    let changes: Vec<&DiffChunk> = chunks.iter().filter(|c| c.tag != DiffTag::Equal).collect();
    if changes.is_empty() {
        return out;
    }

    // Build hunks: merge nearby changes
    let mut hunks: Vec<(usize, usize, usize, usize, Vec<&DiffChunk>)> = Vec::new();
    for &ch in &changes {
        let ctx_start_a = ch.start_a.saturating_sub(context);
        let ctx_start_b = ch.start_b.saturating_sub(context);
        let ctx_end_a = (ch.end_a + context).min(left_lines.len());
        let ctx_end_b = (ch.end_b + context).min(right_lines.len());

        if let Some(last) = hunks.last_mut() {
            // Merge if overlapping
            if ctx_start_a <= last.1 {
                last.1 = ctx_end_a;
                last.3 = ctx_end_b;
                last.4.push(ch);
                continue;
            }
        }
        hunks.push((ctx_start_a, ctx_end_a, ctx_start_b, ctx_end_b, vec![ch]));
    }

    for (hunk_start_a, hunk_end_a, hunk_start_b, hunk_end_b, hunk_chunks) in &hunks {
        let count_a = hunk_end_a - hunk_start_a;
        let count_b = hunk_end_b - hunk_start_b;
        let _ = writeln!(
            out,
            "@@ -{},{count_a} +{},{count_b} @@",
            hunk_start_a + 1,
            hunk_start_b + 1
        );

        let mut pos_a = *hunk_start_a;
        for ch in hunk_chunks {
            // Context lines before this change
            while pos_a < ch.start_a {
                if let Some(line) = left_lines.get(pos_a) {
                    let _ = writeln!(out, " {line}");
                }
                pos_a += 1;
            }
            // Removed lines
            for i in ch.start_a..ch.end_a {
                if let Some(line) = left_lines.get(i) {
                    let _ = writeln!(out, "-{line}");
                }
            }
            // Added lines
            for i in ch.start_b..ch.end_b {
                if let Some(line) = right_lines.get(i) {
                    let _ = writeln!(out, "+{line}");
                }
            }
            pos_a = ch.end_a;
        }
        // Trailing context
        while pos_a < *hunk_end_a {
            if let Some(line) = left_lines.get(pos_a) {
                let _ = writeln!(out, " {line}");
            }
            pos_a += 1;
        }
    }
    out
}

// ─── Row encoding ──────────────────────────────────────────────────────────
// Fields: STATUS \x1f NAME \x1f IS_DIR \x1f REL_PATH \x1f L_SIZE \x1f L_MTIME \x1f R_SIZE \x1f R_MTIME

#[allow(clippy::too_many_arguments)]
fn encode_row(
    status: FileStatus,
    name: &str,
    is_dir: bool,
    rel_path: &str,
    left_size: Option<u64>,
    left_mtime: Option<SystemTime>,
    right_size: Option<u64>,
    right_mtime: Option<SystemTime>,
) -> String {
    let s = status.code();
    let d = if is_dir { "1" } else { "0" };
    let ls = left_size.map(format_size).unwrap_or_default();
    let lm = left_mtime.map(format_mtime).unwrap_or_default();
    let rs = right_size.map(format_size).unwrap_or_default();
    let rm = right_mtime.map(format_mtime).unwrap_or_default();
    format!("{s}{SEP}{name}{SEP}{d}{SEP}{rel_path}{SEP}{ls}{SEP}{lm}{SEP}{rs}{SEP}{rm}")
}

fn decode_field(raw: &str, index: usize) -> &str {
    raw.splitn(8, SEP).nth(index).unwrap_or("")
}

fn decode_status(raw: &str) -> &str {
    decode_field(raw, 0)
}
fn decode_name(raw: &str) -> &str {
    decode_field(raw, 1)
}
fn decode_is_dir(raw: &str) -> bool {
    decode_field(raw, 2) == "1"
}
fn decode_rel_path(raw: &str) -> &str {
    decode_field(raw, 3)
}

// ─── Directory copy helper ──────────────────────────────────────────────────

fn copy_path_recursive(src: &Path, dst: &Path) -> std::io::Result<()> {
    if src.is_file() {
        if let Some(parent) = dst.parent() {
            fs::create_dir_all(parent)?;
        }
        fs::copy(src, dst)?;
    } else if src.is_dir() {
        for entry in walkdir::WalkDir::new(src) {
            let entry = entry.map_err(std::io::Error::other)?;
            let rel = entry.path().strip_prefix(src).unwrap();
            let target = dst.join(rel);
            if entry.file_type().is_dir() {
                fs::create_dir_all(&target)?;
            } else {
                if let Some(parent) = target.parent() {
                    fs::create_dir_all(parent)?;
                }
                fs::copy(entry.path(), &target)?;
            }
        }
    }
    Ok(())
}

// ─── Directory scanning ────────────────────────────────────────────────────

fn read_dir_entries(dir: &Path, dir_filters: &[String]) -> BTreeMap<String, DirMeta> {
    let mut map = BTreeMap::new();
    if let Ok(rd) = fs::read_dir(dir) {
        for entry in rd.filter_map(Result::ok) {
            let Ok(name) = entry.file_name().into_string() else {
                continue;
            };
            if dir_filters.iter().any(|f| f == &name) {
                continue;
            }
            let meta = entry.metadata().ok();
            let is_dir = entry.path().is_dir();
            map.insert(
                name,
                DirMeta {
                    size: meta.as_ref().map(std::fs::Metadata::len),
                    mtime: meta.as_ref().and_then(|m| m.modified().ok()),
                    is_dir,
                },
            );
        }
    }
    map
}

/// Recursively compare two directory trees.
/// Populates `children_map[rel_path]` with a `ListStore` for each directory level.
/// Returns (`store_for_this_level`, `aggregate_status`).
fn scan_level(
    left_root: &Path,
    right_root: &Path,
    rel: &str,
    children_map: &mut HashMap<String, ListStore>,
    dir_filters: &[String],
) -> (ListStore, FileStatus) {
    let left_dir = if rel.is_empty() {
        left_root.to_path_buf()
    } else {
        left_root.join(rel)
    };
    let right_dir = if rel.is_empty() {
        right_root.to_path_buf()
    } else {
        right_root.join(rel)
    };

    let left_entries = read_dir_entries(&left_dir, dir_filters);
    let right_entries = read_dir_entries(&right_dir, dir_filters);
    let all: BTreeSet<&String> = left_entries.keys().chain(right_entries.keys()).collect();

    // Sort: directories first, then files, alphabetically within each group
    let mut names: Vec<(&String, bool)> = all
        .iter()
        .map(|n| {
            let is_dir = left_entries.get(*n).is_some_and(|m| m.is_dir)
                || right_entries.get(*n).is_some_and(|m| m.is_dir);
            (*n, is_dir)
        })
        .collect();
    names.sort_by(|a, b| b.1.cmp(&a.1).then_with(|| a.0.cmp(b.0)));

    let store = ListStore::new::<StringObject>();
    let mut all_same = true;

    for (name, is_dir) in &names {
        let in_left = left_entries.contains_key(*name);
        let in_right = right_entries.contains_key(*name);
        let child_rel = if rel.is_empty() {
            (*name).clone()
        } else {
            format!("{rel}/{name}")
        };

        let status;
        if *is_dir {
            let (child_store, child_agg) =
                scan_level(left_root, right_root, &child_rel, children_map, dir_filters);
            status = if !in_left {
                FileStatus::RightOnly
            } else if !in_right {
                FileStatus::LeftOnly
            } else {
                child_agg
            };
            children_map.insert(child_rel.clone(), child_store);
        } else {
            status = match (in_left, in_right) {
                (true, false) => FileStatus::LeftOnly,
                (false, true) => FileStatus::RightOnly,
                (true, true) => {
                    let lc = fs::read(left_dir.join(name)).unwrap_or_default();
                    let rc = fs::read(right_dir.join(name)).unwrap_or_default();
                    if lc == rc {
                        FileStatus::Same
                    } else {
                        FileStatus::Different
                    }
                }
                _ => unreachable!(),
            };
        }

        if status != FileStatus::Same {
            all_same = false;
        }

        let lm = left_entries.get(*name);
        let rm = right_entries.get(*name);
        let row = encode_row(
            status,
            name,
            *is_dir,
            &child_rel,
            lm.and_then(|m| m.size),
            lm.and_then(|m| m.mtime),
            rm.and_then(|m| m.size),
            rm.and_then(|m| m.mtime),
        );
        store.append(&StringObject::new(&row));
    }

    let agg = if all_same {
        FileStatus::Same
    } else {
        FileStatus::Different
    };
    (store, agg)
}

// ─── CSS helpers ───────────────────────────────────────────────────────────

fn apply_status_class(widget: &impl WidgetExt, status: &str, is_left: bool) {
    for cls in &[
        "diff-changed",
        "diff-deleted",
        "diff-inserted",
        "diff-missing",
    ] {
        widget.remove_css_class(cls);
    }
    if is_left {
        match status {
            "D" => widget.add_css_class("diff-changed"),
            "L" => widget.add_css_class("diff-deleted"),
            "R" => widget.add_css_class("diff-missing"),
            _ => {}
        }
    } else {
        match status {
            "D" => widget.add_css_class("diff-changed"),
            "R" => widget.add_css_class("diff-inserted"),
            "L" => widget.add_css_class("diff-missing"),
            _ => {}
        }
    }
}

// ─── Column factories ──────────────────────────────────────────────────────

/// Name column: `TreeExpander` → Box → [Icon, Label]
fn make_name_factory(is_left: bool) -> SignalListItemFactory {
    let factory = SignalListItemFactory::new();

    factory.connect_setup(|_, list_item| {
        let item = list_item.downcast_ref::<ListItem>().unwrap();
        let expander = TreeExpander::new();
        let hbox = GtkBox::new(Orientation::Horizontal, 4);
        let icon = Image::new();
        let label = Label::new(None);
        label.set_halign(gtk4::Align::Start);
        label.set_hexpand(true);
        hbox.append(&icon);
        hbox.append(&label);
        expander.set_child(Some(&hbox));
        item.set_child(Some(&expander));
    });

    factory.connect_bind(move |_, list_item| {
        let item = list_item.downcast_ref::<ListItem>().unwrap();
        let row = item.item().and_downcast::<TreeListRow>().unwrap();
        let obj = row.item().and_downcast::<StringObject>().unwrap();
        let raw = obj.string();

        let expander = item.child().and_downcast::<TreeExpander>().unwrap();
        expander.set_list_row(Some(&row));

        let hbox = expander.child().and_downcast::<GtkBox>().unwrap();
        let icon = hbox.first_child().and_downcast::<Image>().unwrap();
        let label = icon.next_sibling().and_downcast::<Label>().unwrap();

        let status = decode_status(&raw);
        let name = decode_name(&raw);
        let is_dir = decode_is_dir(&raw);

        icon.set_icon_name(Some(if is_dir {
            "folder-symbolic"
        } else {
            "text-x-generic-symbolic"
        }));
        label.set_label(name);
        apply_status_class(&label, status, is_left);
    });

    factory
}

/// Size / Mtime column: plain label at `field_idx` from encoded row.
fn make_field_factory(is_left: bool, field_idx: usize) -> SignalListItemFactory {
    let factory = SignalListItemFactory::new();

    factory.connect_setup(|_, list_item| {
        let item = list_item.downcast_ref::<ListItem>().unwrap();
        let label = Label::new(None);
        label.set_halign(gtk4::Align::End);
        item.set_child(Some(&label));
    });

    factory.connect_bind(move |_, list_item| {
        let item = list_item.downcast_ref::<ListItem>().unwrap();
        let row = item.item().and_downcast::<TreeListRow>().unwrap();
        let obj = row.item().and_downcast::<StringObject>().unwrap();
        let raw = obj.string();

        let label = item.child().and_downcast::<Label>().unwrap();
        let status = decode_status(&raw);
        let missing = (is_left && status == "R") || (!is_left && status == "L");

        label.set_label(if missing {
            ""
        } else {
            decode_field(&raw, field_idx)
        });
        apply_status_class(&label, status, is_left);
    });

    factory
}

// ─── Diff view helpers ─────────────────────────────────────────────────────

fn setup_diff_tags(buffer: &TextBuffer) {
    let table = buffer.tag_table();
    table.add(
        &TextTag::builder()
            .name("changed")
            .paragraph_background("#fff3cd")
            .build(),
    );
    table.add(
        &TextTag::builder()
            .name("deleted")
            .paragraph_background("#f8d7da")
            .build(),
    );
    table.add(
        &TextTag::builder()
            .name("inserted")
            .paragraph_background("#d4edda")
            .build(),
    );
    table.add(
        &TextTag::builder()
            .name("conflict")
            .paragraph_background("#f5c6cb")
            .build(),
    );
    // Search match highlighting
    table.add(
        &TextTag::builder()
            .name("search-match")
            .background("#ffe066")
            .build(),
    );
    table.add(
        &TextTag::builder()
            .name("search-current")
            .background("#ff9632")
            .build(),
    );
    // Inline word-level highlighting (layered on top of line-level paragraph_background)
    table.add(
        &TextTag::builder()
            .name("inline-changed")
            .background("#f0d080")
            .build(),
    );
    table.add(
        &TextTag::builder()
            .name("inline-deleted")
            .background("#f0b0b0")
            .build(),
    );
    table.add(
        &TextTag::builder()
            .name("inline-inserted")
            .background("#a0dfa0")
            .build(),
    );
}

fn create_source_buffer(file_path: &Path, settings: &Settings) -> TextBuffer {
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

fn remove_diff_tags(buf: &TextBuffer) {
    let start = buf.start_iter();
    let end = buf.end_iter();
    for name in &[
        "changed",
        "deleted",
        "inserted",
        "conflict",
        "inline-changed",
        "inline-deleted",
        "inline-inserted",
    ] {
        if let Some(tag) = buf.tag_table().lookup(name) {
            buf.remove_tag(&tag, &start, &end);
        }
    }
}

/// Detect conflicts: overlapping non-Equal regions from two pairwise diffs on the middle file.
/// `left_chunks`: diff(left, middle) — middle lines are `start_b..end_b`
/// `right_chunks`: diff(middle, right) — middle lines are `start_a..end_a`
fn apply_conflict_tags(
    middle_buf: &TextBuffer,
    left_chunks: &[DiffChunk],
    right_chunks: &[DiffChunk],
) {
    for lc in left_chunks {
        if lc.tag == DiffTag::Equal {
            continue;
        }
        // In the left diff (left vs middle), the middle file range is start_b..end_b
        let l_start = lc.start_b;
        let l_end = lc.end_b;

        for rc in right_chunks {
            if rc.tag == DiffTag::Equal {
                continue;
            }
            // In the right diff (middle vs right), the middle file range is start_a..end_a
            let r_start = rc.start_a;
            let r_end = rc.end_a;

            // Check for overlap (handle zero-length ranges as point insertions)
            let overlap_start = l_start.max(r_start);
            let overlap_end = l_end.max(l_start).min(r_end.max(r_start));

            // Two ranges overlap if they share any lines, or if both are insertions at the same point
            let overlaps = if l_start == l_end && r_start == r_end {
                l_start == r_start
            } else if l_start == l_end {
                l_start >= r_start && l_start < r_end
            } else if r_start == r_end {
                r_start >= l_start && r_start < l_end
            } else {
                overlap_start < overlap_end
            };

            if overlaps {
                // Tag the union of both ranges on the middle buffer
                let tag_start = l_start.min(r_start);
                let tag_end = l_end.max(r_end);
                apply_line_tag(middle_buf, "conflict", tag_start, tag_end);
            }
        }
    }
}

fn apply_line_tag(buffer: &TextBuffer, tag_name: &str, start_line: usize, end_line: usize) {
    let start = buffer.iter_at_line(start_line as i32);
    let end = if (end_line as i32) < buffer.line_count() {
        buffer.iter_at_line(end_line as i32)
    } else {
        Some(buffer.end_iter())
    };
    if let (Some(s), Some(e)) = (start, end) {
        buffer.apply_tag_by_name(tag_name, &s, &e);
    }
}

fn get_line_text(buf: &TextBuffer, line: usize) -> String {
    let start = buf
        .iter_at_line(line as i32)
        .unwrap_or_else(|| buf.start_iter());
    let mut end = start;
    end.forward_to_line_end();
    buf.text(&start, &end, false).to_string()
}

fn apply_char_tag(
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
    // iter_at_line_offset uses character (not byte) offsets, so convert
    let line_text = get_line_text(buf, line);
    let char_start = line_text[..byte_start].chars().count() as i32;
    let char_end = line_text[..byte_end].chars().count() as i32;
    let s = buf.iter_at_line_offset(line as i32, char_start);
    let e = buf.iter_at_line_offset(line as i32, char_end);
    if let (Some(s), Some(e)) = (s, e) {
        buf.apply_tag_by_name(tag_name, &s, &e);
    }
}

fn apply_inline_tags(left_buf: &TextBuffer, right_buf: &TextBuffer, chunk: &DiffChunk) {
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

fn make_info_bar(message: &str) -> GtkBox {
    let bar = GtkBox::new(Orientation::Horizontal, 8);
    bar.add_css_class("info-bar");
    let icon = Image::from_icon_name("dialog-information-symbolic");
    let label = Label::new(Some(message));
    label.set_hexpand(true);
    label.set_halign(gtk4::Align::Start);
    let hide_btn = Button::with_label("Hide");
    hide_btn.add_css_class("raised");
    bar.append(&icon);
    bar.append(&label);
    bar.append(&hide_btn);
    let bar_ref = bar.clone();
    hide_btn.connect_clicked(move |_| bar_ref.set_visible(false));
    bar
}

fn shortened_path(full: &Path) -> String {
    let components: Vec<_> = full.components().collect();
    if components.len() <= 2 {
        return full.display().to_string();
    }
    let tail: std::path::PathBuf = components[components.len() - 2..].iter().collect();
    format!("\u{2026}/{}", tail.display())
}

struct DiffPane {
    container: GtkBox,
    text_view: TextView,
    scroll: ScrolledWindow,
    save_btn: Button,
    path_label: Label,
}

fn make_diff_pane(
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
    sv.set_show_line_numbers(settings.show_line_numbers);
    sv.set_highlight_current_line(settings.highlight_current_line);
    sv.set_tab_width(settings.tab_width);
    sv.set_hexpand(true);
    update_font_css(settings);
    sv.add_css_class("meld-editor");

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
    let display_name = label_override.map_or_else(|| shortened_path(file_path), String::from);
    let path_label = Label::new(Some(&display_name));
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
    let path_owned = file_path.to_path_buf();
    let save_btn_ref = save_btn.clone();
    save_btn.connect_clicked(move |_| {
        SAVING.store(true, std::sync::atomic::Ordering::Relaxed);
        let text = buf_clone.text(&buf_clone.start_iter(), &buf_clone.end_iter(), false);
        let _ = fs::write(&path_owned, text.as_str());
        save_btn_ref.set_sensitive(false);
        // Clear the flag after a short delay so the watcher event is ignored
        gtk4::glib::timeout_add_local_once(Duration::from_millis(600), || {
            SAVING.store(false, std::sync::atomic::Ordering::Relaxed);
        });
    });

    let vbox = GtkBox::new(Orientation::Vertical, 0);
    vbox.append(&header);
    if let Some(msg) = info {
        vbox.append(&make_info_bar(msg));
    }
    vbox.append(&scroll);

    DiffPane {
        container: vbox,
        text_view: tv,
        scroll,
        save_btn,
        path_label,
    }
}

// ─── Gutter (link map) ────────────────────────────────────────────────────

fn line_to_gutter_y(
    tv: &TextView,
    buf: &TextBuffer,
    line: usize,
    scroll: &ScrolledWindow,
    gutter: &impl IsA<gtk4::Widget>,
) -> f64 {
    // Use line_yrange per line so word-wrapped lines get correct positions.
    if let Some(iter) = buf.iter_at_line(line as i32) {
        let (y, _h) = tv.line_yrange(&iter);
        let visible_y = y as f64 - scroll.vadjustment().value();
        let point = gtk4::graphene::Point::new(0.0, visible_y as f32);
        if let Some(out) = scroll.compute_point(gutter, &point) {
            return out.y() as f64;
        }
    }
    0.0
}

#[allow(clippy::too_many_arguments)]
fn draw_gutter(
    cr: &gtk4::cairo::Context,
    width: f64,
    left_tv: &TextView,
    right_tv: &TextView,
    left_buf: &TextBuffer,
    right_buf: &TextBuffer,
    left_scroll: &ScrolledWindow,
    right_scroll: &ScrolledWindow,
    gutter: &DrawingArea,
    chunks: &[DiffChunk],
) {
    for chunk in chunks {
        if chunk.tag == DiffTag::Equal {
            continue;
        }

        let lt = line_to_gutter_y(left_tv, left_buf, chunk.start_a, left_scroll, gutter);
        let lb = line_to_gutter_y(left_tv, left_buf, chunk.end_a, left_scroll, gutter);
        let rt = line_to_gutter_y(right_tv, right_buf, chunk.start_b, right_scroll, gutter);
        let rb = line_to_gutter_y(right_tv, right_buf, chunk.end_b, right_scroll, gutter);

        let (r, g, b) = match chunk.tag {
            DiffTag::Replace => (0.45, 0.62, 0.81),
            DiffTag::Delete => (0.96, 0.47, 0.0),
            DiffTag::Insert => (0.45, 0.82, 0.09),
            DiffTag::Equal => continue,
        };

        // Filled band
        cr.set_source_rgba(r, g, b, 0.3);
        cr.move_to(0.0, lt);
        cr.curve_to(width * 0.5, lt, width * 0.5, rt, width, rt);
        cr.line_to(width, rb);
        cr.curve_to(width * 0.5, rb, width * 0.5, lb, 0.0, lb);
        cr.close_path();
        let _ = cr.fill();

        // Edge-aligned arrows (meld style)
        let left_mid = f64::midpoint(lt, lb);
        let right_mid = f64::midpoint(rt, rb);
        draw_edge_arrow(cr, 2.0, left_mid, true, r, g, b);
        draw_edge_arrow(cr, width - 2.0, right_mid, false, r, g, b);
    }
}

#[allow(clippy::many_single_char_names)]
fn draw_edge_arrow(
    cr: &gtk4::cairo::Context,
    x: f64,
    y: f64,
    points_right: bool,
    r: f64,
    g: f64,
    b: f64,
) {
    let size = 5.0;
    cr.set_source_rgba(r, g, b, 0.9);
    if points_right {
        cr.move_to(x, y - size);
        cr.line_to(x + size * 1.5, y);
        cr.line_to(x, y + size);
    } else {
        cr.move_to(x, y - size);
        cr.line_to(x - size * 1.5, y);
        cr.line_to(x, y + size);
    }
    cr.close_path();
    let _ = cr.fill();
}

fn get_lines_text(buf: &TextBuffer, start_line: usize, end_line: usize) -> String {
    if start_line >= end_line {
        return String::new();
    }
    let start = buf
        .iter_at_line(start_line as i32)
        .unwrap_or(buf.start_iter());
    let end = if (end_line as i32) < buf.line_count() {
        buf.iter_at_line(end_line as i32).unwrap_or(buf.end_iter())
    } else {
        buf.end_iter()
    };
    buf.text(&start, &end, false).to_string()
}

fn copy_chunk(
    src_buf: &TextBuffer,
    src_start: usize,
    src_end: usize,
    dst_buf: &TextBuffer,
    dst_start: usize,
    dst_end: usize,
) {
    let src_text = get_lines_text(src_buf, src_start, src_end);
    let mut ds = dst_buf
        .iter_at_line(dst_start as i32)
        .unwrap_or(dst_buf.start_iter());
    let mut de = if (dst_end as i32) < dst_buf.line_count() {
        dst_buf
            .iter_at_line(dst_end as i32)
            .unwrap_or(dst_buf.end_iter())
    } else {
        dst_buf.end_iter()
    };
    dst_buf.delete(&mut ds, &mut de);
    dst_buf.insert(&mut ds, &src_text);
}

fn refresh_diff(
    left_buf: &TextBuffer,
    right_buf: &TextBuffer,
    chunks: &Rc<RefCell<Vec<DiffChunk>>>,
    on_complete: impl Fn() + 'static,
    ignore_blanks: bool,
    ignore_whitespace: bool,
) {
    let lt = left_buf
        .text(&left_buf.start_iter(), &left_buf.end_iter(), false)
        .to_string();
    let rt = right_buf
        .text(&right_buf.start_iter(), &right_buf.end_iter(), false)
        .to_string();

    remove_diff_tags(left_buf);
    remove_diff_tags(right_buf);

    let lb = left_buf.clone();
    let rb = right_buf.clone();
    let ch = chunks.clone();

    gtk4::glib::spawn_future_local(async move {
        let (lt_cmp, lt_map) = filter_for_diff(&lt, ignore_whitespace, ignore_blanks);
        let (rt_cmp, rt_map) = filter_for_diff(&rt, ignore_whitespace, ignore_blanks);
        let lt_total = lt.lines().count();
        let rt_total = rt.lines().count();
        let new_chunks = if lt_cmp == rt_cmp {
            Vec::new()
        } else {
            let raw = gio::spawn_blocking(move || myers::diff_lines(&lt_cmp, &rt_cmp))
                .await
                .unwrap_or_default();
            remap_chunks(raw, &lt_map, lt_total, &rt_map, rt_total)
        };
        apply_diff_tags(&lb, &rb, &new_chunks);
        *ch.borrow_mut() = new_chunks;
        on_complete();
    });
}

#[allow(clippy::too_many_arguments)]
fn handle_gutter_click(
    x: f64,
    y: f64,
    width: f64,
    left_tv: &TextView,
    right_tv: &TextView,
    left_buf: &TextBuffer,
    right_buf: &TextBuffer,
    left_scroll: &ScrolledWindow,
    right_scroll: &ScrolledWindow,
    gutter: &DrawingArea,
    chunks: &Rc<RefCell<Vec<DiffChunk>>>,
) {
    let snapshot: Vec<DiffChunk> = chunks.borrow().clone();
    for chunk in &snapshot {
        if chunk.tag == DiffTag::Equal {
            continue;
        }
        let lt = line_to_gutter_y(left_tv, left_buf, chunk.start_a, left_scroll, gutter);
        let lb = line_to_gutter_y(left_tv, left_buf, chunk.end_a, left_scroll, gutter);
        let rt = line_to_gutter_y(right_tv, right_buf, chunk.start_b, right_scroll, gutter);
        let rb = line_to_gutter_y(right_tv, right_buf, chunk.end_b, right_scroll, gutter);
        let left_mid = f64::midpoint(lt, lb);
        let right_mid = f64::midpoint(rt, rb);

        let hit = 12.0_f64;
        // → arrow at left edge: copy left to right
        if (x - 2.0).powi(2) + (y - left_mid).powi(2) < hit * hit {
            copy_chunk(
                left_buf,
                chunk.start_a,
                chunk.end_a,
                right_buf,
                chunk.start_b,
                chunk.end_b,
            );
            return;
        }
        // ← arrow at right edge: copy right to left
        if (x - (width - 2.0)).powi(2) + (y - right_mid).powi(2) < hit * hit {
            copy_chunk(
                right_buf,
                chunk.start_b,
                chunk.end_b,
                left_buf,
                chunk.start_a,
                chunk.end_a,
            );
            return;
        }
    }
}

// ─── Search helpers ─────────────────────────────────────────────────────────

fn clear_search_tags(buf: &TextBuffer) {
    let start = buf.start_iter();
    let end = buf.end_iter();
    for name in &["search-match", "search-current"] {
        if let Some(tag) = buf.tag_table().lookup(name) {
            buf.remove_tag(&tag, &start, &end);
        }
    }
}

/// Highlight all occurrences of `needle` in `buf`. Returns match count.
fn highlight_search_matches(buf: &TextBuffer, needle: &str) -> usize {
    clear_search_tags(buf);
    if needle.is_empty() {
        return 0;
    }
    let mut count = 0;
    let mut iter = buf.start_iter();
    while let Some((match_start, match_end)) =
        iter.forward_search(needle, TextSearchFlags::CASE_INSENSITIVE, None)
    {
        buf.apply_tag_by_name("search-match", &match_start, &match_end);
        count += 1;
        iter = match_end;
    }
    count
}

/// Find the next (or previous) match starting from `from`. Returns (start, end) iters.
fn find_next_match(
    buf: &TextBuffer,
    needle: &str,
    from: &gtk4::TextIter,
    forward: bool,
) -> Option<(gtk4::TextIter, gtk4::TextIter)> {
    if needle.is_empty() {
        return None;
    }
    if forward {
        let result = from.forward_search(needle, TextSearchFlags::CASE_INSENSITIVE, None);
        if result.is_some() {
            return result;
        }
        // Wrap around
        buf.start_iter()
            .forward_search(needle, TextSearchFlags::CASE_INSENSITIVE, None)
    } else {
        let result = from.backward_search(needle, TextSearchFlags::CASE_INSENSITIVE, None);
        if result.is_some() {
            return result;
        }
        // Wrap around
        buf.end_iter()
            .backward_search(needle, TextSearchFlags::CASE_INSENSITIVE, None)
    }
}

// ─── Chunk map helpers ──────────────────────────────────────────────────────

fn draw_chunk_map(
    cr: &gtk4::cairo::Context,
    _width: f64,
    height: f64,
    total_lines: i32,
    scroll: &ScrolledWindow,
    chunks: &[DiffChunk],
    is_left: bool,
) {
    if total_lines <= 0 || height <= 0.0 {
        return;
    }
    let lines = total_lines as f64;

    for chunk in chunks {
        if chunk.tag == DiffTag::Equal {
            continue;
        }

        let (start, end) = if is_left {
            (chunk.start_a, chunk.end_a)
        } else {
            (chunk.start_b, chunk.end_b)
        };

        let y_start = (start as f64 / lines) * height;
        let y_end = (end as f64 / lines) * height;
        let rect_h = (y_end - y_start).max(2.0);

        let (r, g, b) = match chunk.tag {
            DiffTag::Replace => (0.45, 0.62, 0.81),
            DiffTag::Delete => (0.96, 0.47, 0.0),
            DiffTag::Insert => (0.45, 0.82, 0.09),
            DiffTag::Equal => continue,
        };

        cr.set_source_rgba(r, g, b, 0.7);
        cr.rectangle(1.0, y_start, 10.0, rect_h);
        let _ = cr.fill();
    }

    // Viewport indicator
    let adj = scroll.vadjustment();
    if adj.upper() > 0.0 {
        let view_start = (adj.value() / adj.upper()) * height;
        let view_h = (adj.page_size() / adj.upper()) * height;
        cr.set_source_rgba(0.0, 0.0, 0.0, 0.12);
        cr.rectangle(0.0, view_start, 12.0, view_h);
        let _ = cr.fill();
        cr.set_source_rgba(0.0, 0.0, 0.0, 0.25);
        cr.rectangle(0.0, view_start, 12.0, 1.0);
        let _ = cr.fill();
        cr.rectangle(0.0, view_start + view_h - 1.0, 12.0, 1.0);
        let _ = cr.fill();
    }
}

// ─── Shared diff view construction ─────────────────────────────────────────

struct DiffViewResult {
    widget: GtkBox,
    left_buf: TextBuffer,
    right_buf: TextBuffer,
    left_save: Button,
    right_save: Button,
    action_group: gio::SimpleActionGroup,
}

/// Build a complete diff view widget for two files.
/// Returns the top-level widget (toolbar + diff panes) and associated state.
fn build_diff_view(
    left_path: &Path,
    right_path: &Path,
    labels: &[String],
    settings: &Rc<RefCell<Settings>>,
) -> DiffViewResult {
    let s = settings.borrow();
    let (left_content, left_binary) = read_file_content(left_path);
    let (right_content, right_binary) = read_file_content(right_path);
    let any_binary = left_binary || right_binary;

    let left_buf = create_source_buffer(left_path, &s);
    let right_buf = create_source_buffer(right_path, &s);
    left_buf.set_text(&left_content);
    right_buf.set_text(&right_content);

    let identical = !any_binary && left_content == right_content;
    let chunks = Rc::new(RefCell::new(Vec::new()));

    let info_msg = if any_binary {
        Some("Binary file — cannot display diff")
    } else if identical {
        Some("Files are identical")
    } else {
        None
    };

    let left_label = labels.first().map(String::as_str);
    let right_label = labels.get(1).map(String::as_str);
    let left_pane = make_diff_pane(&left_buf, left_path, info_msg, left_label, &s);
    let right_pane = make_diff_pane(&right_buf, right_path, info_msg, right_label, &s);
    drop(s);

    if any_binary {
        left_pane.text_view.set_editable(false);
        right_pane.text_view.set_editable(false);
    }

    // Track which text view was last focused (for undo/redo, find, go-to-line)
    let active_view: Rc<RefCell<TextView>> = Rc::new(RefCell::new(left_pane.text_view.clone()));
    {
        let av = active_view.clone();
        let tv = left_pane.text_view.clone();
        let fc = EventControllerFocus::new();
        fc.connect_enter(move |_| {
            *av.borrow_mut() = tv.clone();
        });
        left_pane.text_view.add_controller(fc);
    }
    {
        let av = active_view.clone();
        let tv = right_pane.text_view.clone();
        let fc = EventControllerFocus::new();
        fc.connect_enter(move |_| {
            *av.borrow_mut() = tv.clone();
        });
        right_pane.text_view.add_controller(fc);
    }

    // Gutter (link map between left and right)
    let gutter = DrawingArea::new();
    gutter.set_content_width(48);
    gutter.set_vexpand(true);

    // Draw connecting bands + arrows
    {
        let ltv = left_pane.text_view.clone();
        let rtv = right_pane.text_view.clone();
        let lb = left_buf.clone();
        let rb = right_buf.clone();
        let ls = left_pane.scroll.clone();
        let rs = right_pane.scroll.clone();
        let ch = chunks.clone();
        gutter.set_draw_func(move |area, cr, width, _height| {
            draw_gutter(
                cr,
                width as f64,
                &ltv,
                &rtv,
                &lb,
                &rb,
                &ls,
                &rs,
                area,
                &ch.borrow(),
            );
        });
    }

    // Click handler for arrows
    {
        let gesture = GestureClick::new();
        let ltv = left_pane.text_view.clone();
        let rtv = right_pane.text_view.clone();
        let lb = left_buf.clone();
        let rb = right_buf.clone();
        let ls = left_pane.scroll.clone();
        let rs = right_pane.scroll.clone();
        let ch = chunks.clone();
        let g = gutter.clone();
        gesture.connect_pressed(move |_, _, x, y| {
            handle_gutter_click(
                x,
                y,
                g.width() as f64,
                &ltv,
                &rtv,
                &lb,
                &rb,
                &ls,
                &rs,
                &g,
                &ch,
            );
        });
        gutter.add_controller(gesture);
    }

    // Right-click context menu for gutter
    {
        let gutter_pending: Rc<Cell<Option<usize>>> = Rc::new(Cell::new(None));
        let gutter_ctx = gio::SimpleActionGroup::new();
        {
            let action = gio::SimpleAction::new("copy-left-right", None);
            let pc = gutter_pending.clone();
            let ch = chunks.clone();
            let lb = left_buf.clone();
            let rb = right_buf.clone();
            action.connect_activate(move |_, _| {
                if let Some(idx) = pc.get() {
                    let snapshot = ch.borrow();
                    if let Some(c) = snapshot.get(idx) {
                        copy_chunk(&lb, c.start_a, c.end_a, &rb, c.start_b, c.end_b);
                    }
                }
            });
            gutter_ctx.add_action(&action);
        }
        {
            let action = gio::SimpleAction::new("copy-right-left", None);
            let pc = gutter_pending.clone();
            let ch = chunks.clone();
            let lb = left_buf.clone();
            let rb = right_buf.clone();
            action.connect_activate(move |_, _| {
                if let Some(idx) = pc.get() {
                    let snapshot = ch.borrow();
                    if let Some(c) = snapshot.get(idx) {
                        copy_chunk(&rb, c.start_b, c.end_b, &lb, c.start_a, c.end_a);
                    }
                }
            });
            gutter_ctx.add_action(&action);
        }
        gutter.insert_action_group("gutter", Some(&gutter_ctx));

        let gutter_menu = gio::Menu::new();
        gutter_menu.append(
            Some("Copy Left \u{2192} Right"),
            Some("gutter.copy-left-right"),
        );
        gutter_menu.append(
            Some("Copy Right \u{2192} Left"),
            Some("gutter.copy-right-left"),
        );
        let gutter_popover = PopoverMenu::from_model(Some(&gutter_menu));
        gutter_popover.set_parent(&gutter);
        gutter_popover.set_has_arrow(false);

        let gesture = GestureClick::new();
        gesture.set_button(3);
        let ltv = left_pane.text_view.clone();
        let rtv = right_pane.text_view.clone();
        let lb = left_buf.clone();
        let rb = right_buf.clone();
        let ls = left_pane.scroll.clone();
        let rs = right_pane.scroll.clone();
        let ch = chunks.clone();
        let g = gutter.clone();
        let pc = gutter_pending.clone();
        let pop = gutter_popover;
        gesture.connect_pressed(move |_, _, x, y| {
            let snapshot = ch.borrow();
            for (idx, chunk) in snapshot.iter().enumerate() {
                if chunk.tag == DiffTag::Equal {
                    continue;
                }
                let lt = line_to_gutter_y(&ltv, &lb, chunk.start_a, &ls, &g);
                let lb_y = line_to_gutter_y(&ltv, &lb, chunk.end_a, &ls, &g);
                let rt = line_to_gutter_y(&rtv, &rb, chunk.start_b, &rs, &g);
                let rb_y = line_to_gutter_y(&rtv, &rb, chunk.end_b, &rs, &g);
                let top = lt.min(rt) - 6.0;
                let bottom = lb_y.max(rb_y) + 6.0;
                if y >= top && y <= bottom {
                    pc.set(Some(idx));
                    pop.set_pointing_to(Some(&gtk4::gdk::Rectangle::new(x as i32, y as i32, 1, 1)));
                    pop.popup();
                    return;
                }
            }
        });
        gutter.add_controller(gesture);
    }

    // Text filter state (created early so connect_changed can use it)
    let ignore_blanks: Rc<Cell<bool>> = Rc::new(Cell::new(false));
    let ignore_whitespace: Rc<Cell<bool>> = Rc::new(Cell::new(false));

    // Scroll synchronization
    setup_scroll_sync(&left_pane.scroll, &right_pane.scroll, &gutter);

    // ── Toolbar with chunk navigation ───────────────────────────
    let current_chunk: Rc<Cell<Option<usize>>> = Rc::new(Cell::new(None));

    let chunk_label = Label::new(None);
    chunk_label.add_css_class("chunk-label");
    update_chunk_label(&chunk_label, &chunks.borrow(), None);

    let prev_btn = Button::from_icon_name("go-up-symbolic");
    prev_btn.set_tooltip_text(Some("Previous change (Alt+Up / Ctrl+E)"));
    let next_btn = Button::from_icon_name("go-down-symbolic");
    next_btn.set_tooltip_text(Some("Next change (Alt+Down / Ctrl+D)"));

    let nav_box = GtkBox::new(Orientation::Horizontal, 0);
    nav_box.add_css_class("linked");
    nav_box.append(&prev_btn);
    nav_box.append(&next_btn);

    // Undo/Redo buttons
    let undo_btn = Button::from_icon_name("edit-undo-symbolic");
    undo_btn.set_tooltip_text(Some("Undo (Ctrl+Z)"));
    let redo_btn = Button::from_icon_name("edit-redo-symbolic");
    redo_btn.set_tooltip_text(Some("Redo (Ctrl+Shift+Z)"));
    let undo_redo_box = GtkBox::new(Orientation::Horizontal, 0);
    undo_redo_box.add_css_class("linked");
    undo_redo_box.append(&undo_btn);
    undo_redo_box.append(&redo_btn);

    {
        let av = active_view.clone();
        undo_btn.connect_clicked(move |_| {
            let buf = av.borrow().buffer();
            if buf.can_undo() {
                buf.undo();
            }
        });
    }
    {
        let av = active_view.clone();
        redo_btn.connect_clicked(move |_| {
            let buf = av.borrow().buffer();
            if buf.can_redo() {
                buf.redo();
            }
        });
    }

    // Go to line entry (hidden by default, shown by Ctrl+L)
    let goto_entry = Entry::new();
    goto_entry.set_placeholder_text(Some("Line #"));
    goto_entry.set_width_chars(8);
    goto_entry.add_css_class("goto-entry");
    goto_entry.set_visible(false);
    {
        let av = active_view.clone();
        let ls = left_pane.scroll.clone();
        let entry = goto_entry.clone();
        goto_entry.connect_activate(move |e| {
            if let Ok(line) = e.text().trim().parse::<usize>() {
                let tv = av.borrow().clone();
                let buf = tv.buffer();
                let target = line.saturating_sub(1); // 1-indexed to 0-indexed
                scroll_to_line(&tv, &buf, target, &ls);
                if let Some(iter) = buf.iter_at_line(target as i32) {
                    buf.place_cursor(&iter);
                }
            }
            e.set_visible(false);
            entry.set_text("");
        });
    }
    {
        let entry = goto_entry.clone();
        let key_ctl = EventControllerKey::new();
        key_ctl.connect_key_pressed(move |_, key, _, _| {
            if key == gtk4::gdk::Key::Escape {
                entry.set_visible(false);
                entry.set_text("");
                return gtk4::glib::Propagation::Stop;
            }
            gtk4::glib::Propagation::Proceed
        });
        goto_entry.add_controller(key_ctl);
    }

    let toolbar = GtkBox::new(Orientation::Horizontal, 8);
    toolbar.set_margin_start(6);
    toolbar.set_margin_end(6);
    toolbar.set_margin_top(4);
    toolbar.set_margin_bottom(4);
    toolbar.append(&undo_redo_box);
    let swap_btn = Button::from_icon_name("object-flip-horizontal-symbolic");
    swap_btn.set_tooltip_text(Some("Swap panes"));

    // Text filter toggles
    let blank_toggle = ToggleButton::with_label("Blanks");
    blank_toggle.set_tooltip_text(Some("Ignore blank lines"));
    let ws_toggle = ToggleButton::with_label("Spaces");
    ws_toggle.set_tooltip_text(Some("Ignore whitespace differences"));

    let filter_box = GtkBox::new(Orientation::Horizontal, 0);
    filter_box.add_css_class("linked");
    filter_box.append(&blank_toggle);
    filter_box.append(&ws_toggle);

    let patch_btn = Button::from_icon_name("document-save-as-symbolic");
    patch_btn.set_tooltip_text(Some("Export patch (Ctrl+Shift+P)"));

    toolbar.append(&nav_box);
    toolbar.append(&chunk_label);
    toolbar.append(&goto_entry);
    toolbar.append(&filter_box);
    toolbar.append(&patch_btn);
    toolbar.append(&swap_btn);
    let prefs_btn = Button::from_icon_name("preferences-system-symbolic");
    prefs_btn.set_tooltip_text(Some("Preferences (Ctrl+,)"));
    prefs_btn.set_action_name(Some("win.prefs"));
    toolbar.append(&prefs_btn);

    // Swap panes: swap buffer text + labels, re-diff happens via connect_changed
    {
        let lb = left_buf.clone();
        let rb = right_buf.clone();
        let ll = left_pane.path_label.clone();
        let rl = right_pane.path_label.clone();
        swap_btn.connect_clicked(move |_| {
            let lt = lb.text(&lb.start_iter(), &lb.end_iter(), false).to_string();
            let rt = rb.text(&rb.start_iter(), &rb.end_iter(), false).to_string();
            lb.set_text(&rt);
            rb.set_text(&lt);
            let ll_text = ll.text().to_string();
            let rl_text = rl.text().to_string();
            ll.set_text(&rl_text);
            rl.set_text(&ll_text);
        });
    }

    // Prev chunk
    {
        let ch = chunks.clone();
        let cur = current_chunk.clone();
        let ltv = left_pane.text_view.clone();
        let lb = left_buf.clone();
        let ls = left_pane.scroll.clone();
        let lbl = chunk_label.clone();
        prev_btn.connect_clicked(move |_| {
            navigate_chunk(&ch.borrow(), &cur, -1, &ltv, &lb, &ls);
            update_chunk_label(&lbl, &ch.borrow(), cur.get());
        });
    }

    // Next chunk
    {
        let ch = chunks.clone();
        let cur = current_chunk.clone();
        let ltv = left_pane.text_view.clone();
        let lb = left_buf.clone();
        let ls = left_pane.scroll.clone();
        let lbl = chunk_label.clone();
        next_btn.connect_clicked(move |_| {
            navigate_chunk(&ch.borrow(), &cur, 1, &ltv, &lb, &ls);
            update_chunk_label(&lbl, &ch.borrow(), cur.get());
        });
    }

    // ── Chunk maps (overview strips) ─────────────────────────────
    let left_chunk_map = DrawingArea::new();
    left_chunk_map.set_content_width(12);
    left_chunk_map.set_vexpand(true);
    {
        let lb = left_buf.clone();
        let ls = left_pane.scroll.clone();
        let ch = chunks.clone();
        left_chunk_map.set_draw_func(move |_area, cr, _w, h| {
            draw_chunk_map(cr, 12.0, h as f64, lb.line_count(), &ls, &ch.borrow(), true);
        });
    }
    // Click to jump
    {
        let gesture = GestureClick::new();
        let ls = left_pane.scroll.clone();
        let lm = left_chunk_map.clone();
        gesture.connect_pressed(move |_, _, _x, y| {
            let h = lm.height() as f64;
            if h > 0.0 {
                let adj = ls.vadjustment();
                let target = (y / h) * adj.upper() - adj.page_size() / 2.0;
                adj.set_value(target.max(0.0));
            }
        });
        left_chunk_map.add_controller(gesture);
    }

    let right_chunk_map = DrawingArea::new();
    right_chunk_map.set_content_width(12);
    right_chunk_map.set_vexpand(true);
    {
        let rb = right_buf.clone();
        let rs = right_pane.scroll.clone();
        let ch = chunks.clone();
        right_chunk_map.set_draw_func(move |_area, cr, _w, h| {
            draw_chunk_map(
                cr,
                12.0,
                h as f64,
                rb.line_count(),
                &rs,
                &ch.borrow(),
                false,
            );
        });
    }
    {
        let gesture = GestureClick::new();
        let rs = right_pane.scroll.clone();
        let rm = right_chunk_map.clone();
        gesture.connect_pressed(move |_, _, _x, y| {
            let h = rm.height() as f64;
            if h > 0.0 {
                let adj = rs.vadjustment();
                let target = (y / h) * adj.upper() - adj.page_size() / 2.0;
                adj.set_value(target.max(0.0));
            }
        });
        right_chunk_map.add_controller(gesture);
    }

    // Redraw chunk maps on scroll and buffer changes
    {
        let lcm = left_chunk_map.clone();
        let rcm = right_chunk_map.clone();
        left_pane
            .scroll
            .vadjustment()
            .connect_value_changed(move |_| {
                lcm.queue_draw();
                rcm.queue_draw();
            });
    }
    {
        let lcm = left_chunk_map.clone();
        let rcm = right_chunk_map.clone();
        right_pane
            .scroll
            .vadjustment()
            .connect_value_changed(move |_| {
                lcm.queue_draw();
                rcm.queue_draw();
            });
    }

    // Re-diff on any buffer change, and update all visuals when diff completes
    {
        let make_on_complete = {
            let g = gutter.clone();
            let lcm = left_chunk_map.clone();
            let rcm = right_chunk_map.clone();
            let lbl = chunk_label.clone();
            let ch = chunks.clone();
            let cur = current_chunk.clone();
            move || {
                let g = g.clone();
                let lcm = lcm.clone();
                let rcm = rcm.clone();
                let lbl = lbl.clone();
                let ch = ch.clone();
                let cur = cur.clone();
                move || {
                    g.queue_draw();
                    lcm.queue_draw();
                    rcm.queue_draw();
                    cur.set(None);
                    update_chunk_label(&lbl, &ch.borrow(), None);
                }
            }
        };

        let pending = Rc::new(Cell::new(false));
        let connect_refresh = |buf: &TextBuffer| {
            let lb = left_buf.clone();
            let rb = right_buf.clone();
            let ch = chunks.clone();
            let p = pending.clone();
            let ib = ignore_blanks.clone();
            let iw = ignore_whitespace.clone();
            let make_cb = make_on_complete.clone();
            buf.connect_changed(move |_| {
                if !p.get() {
                    p.set(true);
                    let lb = lb.clone();
                    let rb = rb.clone();
                    let ch = ch.clone();
                    let p = p.clone();
                    let ib = ib.clone();
                    let iw = iw.clone();
                    let cb = make_cb();
                    gtk4::glib::idle_add_local_once(move || {
                        refresh_diff(&lb, &rb, &ch, cb, ib.get(), iw.get());
                        p.set(false);
                    });
                }
            });
        };
        connect_refresh(&left_buf);
        connect_refresh(&right_buf);

        // Initial async diff (must be after chunk_label + chunk_maps exist)
        if !identical && !any_binary {
            let lb = left_buf.clone();
            let rb = right_buf.clone();
            let ch = chunks.clone();
            let on_complete = make_on_complete();
            gtk4::glib::spawn_future_local(async move {
                let new_chunks =
                    gio::spawn_blocking(move || myers::diff_lines(&left_content, &right_content))
                        .await
                        .unwrap_or_default();
                apply_diff_tags(&lb, &rb, &new_chunks);
                *ch.borrow_mut() = new_chunks;
                on_complete();
            });
        }

        // Toggle handlers for filter buttons
        {
            let ib = ignore_blanks.clone();
            let iw = ignore_whitespace.clone();
            let lb = left_buf.clone();
            let rb = right_buf.clone();
            let ch = chunks.clone();
            let make_cb = make_on_complete.clone();
            blank_toggle.connect_toggled(move |btn| {
                ib.set(btn.is_active());
                refresh_diff(&lb, &rb, &ch, make_cb(), ib.get(), iw.get());
            });
        }
        {
            let ib = ignore_blanks.clone();
            let iw = ignore_whitespace.clone();
            let lb = left_buf.clone();
            let rb = right_buf.clone();
            let ch = chunks.clone();
            ws_toggle.connect_toggled(move |btn| {
                iw.set(btn.is_active());
                refresh_diff(&lb, &rb, &ch, make_on_complete(), ib.get(), iw.get());
            });
        }
    }

    // ── Find bar ──────────────────────────────────────────────────
    let find_entry = Entry::new();
    find_entry.set_placeholder_text(Some("Find (Ctrl+F)"));
    find_entry.set_hexpand(true);

    let replace_entry = Entry::new();
    replace_entry.set_placeholder_text(Some("Replace"));
    replace_entry.set_hexpand(true);

    let find_prev_btn = Button::from_icon_name("go-up-symbolic");
    find_prev_btn.set_tooltip_text(Some("Previous match (Shift+F3)"));
    let find_next_btn = Button::from_icon_name("go-down-symbolic");
    find_next_btn.set_tooltip_text(Some("Next match (F3)"));
    let match_label = Label::new(None);
    match_label.add_css_class("chunk-label");
    let find_close_btn = Button::from_icon_name("window-close-symbolic");
    find_close_btn.set_has_frame(false);
    find_close_btn.set_tooltip_text(Some("Close (Escape)"));

    let replace_btn = Button::with_label("Replace");
    let replace_all_btn = Button::with_label("All");
    let replace_row = GtkBox::new(Orientation::Horizontal, 4);
    replace_row.set_margin_start(6);
    replace_row.set_margin_end(6);
    replace_row.append(&replace_entry);
    replace_row.append(&replace_btn);
    replace_row.append(&replace_all_btn);
    replace_row.set_visible(false);

    let find_nav = GtkBox::new(Orientation::Horizontal, 0);
    find_nav.add_css_class("linked");
    find_nav.append(&find_prev_btn);
    find_nav.append(&find_next_btn);

    let find_row = GtkBox::new(Orientation::Horizontal, 4);
    find_row.set_margin_start(6);
    find_row.set_margin_end(6);
    find_row.append(&find_entry);
    find_row.append(&find_nav);
    find_row.append(&match_label);
    find_row.append(&find_close_btn);

    let find_bar = GtkBox::new(Orientation::Vertical, 2);
    find_bar.add_css_class("find-bar");
    find_bar.append(&find_row);
    find_bar.append(&replace_row);

    let find_revealer = Revealer::new();
    find_revealer.set_child(Some(&find_bar));
    find_revealer.set_reveal_child(false);
    find_revealer.set_transition_type(gtk4::RevealerTransitionType::SlideUp);

    // Search logic: highlight matches when find entry text changes
    {
        let lb = left_buf.clone();
        let rb = right_buf.clone();
        let ml = match_label.clone();
        find_entry.connect_changed(move |e| {
            let needle = e.text().to_string();
            let lc = highlight_search_matches(&lb, &needle);
            let rc = highlight_search_matches(&rb, &needle);
            let total = lc + rc;
            if needle.is_empty() {
                ml.set_label("");
            } else if total == 0 {
                ml.set_label("No matches");
            } else {
                ml.set_label(&format!("{total} matches"));
            }
        });
    }

    // Find next
    {
        let av = active_view.clone();
        let fe = find_entry.clone();
        let ls = left_pane.scroll.clone();
        find_next_btn.connect_clicked(move |_| {
            let needle = fe.text().to_string();
            let tv = av.borrow().clone();
            let buf = tv.buffer();
            let cursor = buf.iter_at_mark(&buf.get_insert());
            if let Some((start, end)) = find_next_match(&buf, &needle, &cursor, true) {
                buf.select_range(&start, &end);
                scroll_to_line(&tv, &buf, start.line() as usize, &ls);
            }
        });
    }

    // Find prev
    {
        let av = active_view.clone();
        let fe = find_entry.clone();
        let ls = left_pane.scroll.clone();
        find_prev_btn.connect_clicked(move |_| {
            let needle = fe.text().to_string();
            let tv = av.borrow().clone();
            let buf = tv.buffer();
            let cursor = buf.iter_at_mark(&buf.get_insert());
            if let Some((start, end)) = find_next_match(&buf, &needle, &cursor, false) {
                buf.select_range(&start, &end);
                scroll_to_line(&tv, &buf, start.line() as usize, &ls);
            }
        });
    }

    // Find entry Enter = find next
    {
        let av = active_view.clone();
        let ls = left_pane.scroll.clone();
        find_entry.connect_activate(move |e| {
            let needle = e.text().to_string();
            let tv = av.borrow().clone();
            let buf = tv.buffer();
            let cursor = buf.iter_at_mark(&buf.get_insert());
            if let Some((start, end)) = find_next_match(&buf, &needle, &cursor, true) {
                buf.select_range(&start, &end);
                scroll_to_line(&tv, &buf, start.line() as usize, &ls);
            }
        });
    }

    // Replace
    {
        let av = active_view.clone();
        let find_e = find_entry.clone();
        let repl_e = replace_entry.clone();
        replace_btn.connect_clicked(move |_| {
            let tv = av.borrow().clone();
            let buf = tv.buffer();
            let (sel_start, sel_end) = buf.selection_bounds().unwrap_or_else(|| {
                let c = buf.iter_at_mark(&buf.get_insert());
                (c, c)
            });
            let selected = buf.text(&sel_start, &sel_end, false).to_string();
            let needle = find_e.text().to_string();
            if !needle.is_empty() && selected.to_lowercase() == needle.to_lowercase() {
                let replacement = repl_e.text().to_string();
                let mut s = sel_start;
                let mut e = sel_end;
                buf.delete(&mut s, &mut e);
                buf.insert(&mut s, &replacement);
            }
        });
    }

    // Replace all
    {
        let lb = left_buf.clone();
        let rb = right_buf.clone();
        let find_e = find_entry.clone();
        let repl_e = replace_entry.clone();
        replace_all_btn.connect_clicked(move |_| {
            let needle = find_e.text().to_string();
            let replacement = repl_e.text().to_string();
            if needle.is_empty() {
                return;
            }
            for buf in [&lb, &rb] {
                let text = buf
                    .text(&buf.start_iter(), &buf.end_iter(), false)
                    .to_string();
                let new_text = text.replace(&needle, &replacement);
                if new_text != text {
                    buf.set_text(&new_text);
                }
            }
        });
    }

    // Close find bar
    {
        let fr = find_revealer.clone();
        let lb = left_buf.clone();
        let rb = right_buf.clone();
        find_close_btn.connect_clicked(move |_| {
            fr.set_reveal_child(false);
            clear_search_tags(&lb);
            clear_search_tags(&rb);
        });
    }

    // Escape in find entry closes find bar
    {
        let fr = find_revealer.clone();
        let lb = left_buf.clone();
        let rb = right_buf.clone();
        let key_ctl = EventControllerKey::new();
        key_ctl.connect_key_pressed(move |_, key, _, _| {
            if key == gtk4::gdk::Key::Escape {
                fr.set_reveal_child(false);
                clear_search_tags(&lb);
                clear_search_tags(&rb);
                return gtk4::glib::Propagation::Stop;
            }
            gtk4::glib::Propagation::Proceed
        });
        find_entry.add_controller(key_ctl);
    }

    // Layout: toolbar + separator + [chunk_map | left pane | gutter | right pane | chunk_map] + find bar
    let diff_row = GtkBox::new(Orientation::Horizontal, 0);
    left_pane.container.set_hexpand(true);
    right_pane.container.set_hexpand(true);
    diff_row.append(&left_chunk_map);
    diff_row.append(&left_pane.container);
    diff_row.append(&gutter);
    diff_row.append(&right_pane.container);
    diff_row.append(&right_chunk_map);
    diff_row.set_vexpand(true);

    let widget = GtkBox::new(Orientation::Vertical, 0);
    widget.append(&toolbar);
    widget.append(&gtk4::Separator::new(Orientation::Horizontal));
    widget.append(&diff_row);
    widget.append(&find_revealer);

    // GAction group for keyboard shortcuts
    let action_group = gio::SimpleActionGroup::new();
    {
        let action = gio::SimpleAction::new("prev-chunk", None);
        let ch = chunks.clone();
        let cur = current_chunk.clone();
        let ltv = left_pane.text_view.clone();
        let lb = left_buf.clone();
        let ls = left_pane.scroll.clone();
        let lbl = chunk_label.clone();
        action.connect_activate(move |_, _| {
            navigate_chunk(&ch.borrow(), &cur, -1, &ltv, &lb, &ls);
            update_chunk_label(&lbl, &ch.borrow(), cur.get());
        });
        action_group.add_action(&action);
    }
    {
        let action = gio::SimpleAction::new("next-chunk", None);
        let ch = chunks.clone();
        let cur = current_chunk.clone();
        let ltv = left_pane.text_view.clone();
        let lb = left_buf.clone();
        let ls = left_pane.scroll.clone();
        let lbl = chunk_label.clone();
        action.connect_activate(move |_, _| {
            navigate_chunk(&ch.borrow(), &cur, 1, &ltv, &lb, &ls);
            update_chunk_label(&lbl, &ch.borrow(), cur.get());
        });
        action_group.add_action(&action);
    }
    // Find action (Ctrl+F)
    {
        let action = gio::SimpleAction::new("find", None);
        let fr = find_revealer.clone();
        let fe = find_entry.clone();
        let rr = replace_row.clone();
        action.connect_activate(move |_, _| {
            rr.set_visible(false);
            fr.set_reveal_child(true);
            fe.grab_focus();
        });
        action_group.add_action(&action);
    }
    // Find-replace action (Ctrl+H)
    {
        let action = gio::SimpleAction::new("find-replace", None);
        let fr = find_revealer.clone();
        let fe = find_entry.clone();
        let rr = replace_row.clone();
        action.connect_activate(move |_, _| {
            rr.set_visible(true);
            fr.set_reveal_child(true);
            fe.grab_focus();
        });
        action_group.add_action(&action);
    }
    // Find next (F3)
    {
        let action = gio::SimpleAction::new("find-next", None);
        let av = active_view.clone();
        let fe = find_entry.clone();
        let ls = left_pane.scroll.clone();
        action.connect_activate(move |_, _| {
            let needle = fe.text().to_string();
            let tv = av.borrow().clone();
            let buf = tv.buffer();
            let cursor = buf.iter_at_mark(&buf.get_insert());
            if let Some((start, end)) = find_next_match(&buf, &needle, &cursor, true) {
                buf.select_range(&start, &end);
                scroll_to_line(&tv, &buf, start.line() as usize, &ls);
            }
        });
        action_group.add_action(&action);
    }
    // Find prev (Shift+F3)
    {
        let action = gio::SimpleAction::new("find-prev", None);
        let av = active_view.clone();
        let fe = find_entry.clone();
        let ls = left_pane.scroll.clone();
        action.connect_activate(move |_, _| {
            let needle = fe.text().to_string();
            let tv = av.borrow().clone();
            let buf = tv.buffer();
            let cursor = buf.iter_at_mark(&buf.get_insert());
            if let Some((start, end)) = find_next_match(&buf, &needle, &cursor, false) {
                buf.select_range(&start, &end);
                scroll_to_line(&tv, &buf, start.line() as usize, &ls);
            }
        });
        action_group.add_action(&action);
    }
    // Go to line (Ctrl+L)
    {
        let action = gio::SimpleAction::new("go-to-line", None);
        let ge = goto_entry.clone();
        action.connect_activate(move |_, _| {
            ge.set_visible(true);
            ge.grab_focus();
        });
        action_group.add_action(&action);
    }

    // Export patch
    {
        let export_patch = {
            let lb = left_buf.clone();
            let rb = right_buf.clone();
            let ch = chunks.clone();
            let ll = left_pane.path_label.clone();
            let rl = right_pane.path_label.clone();
            let pb = patch_btn.clone();
            move || {
                let lt = lb.text(&lb.start_iter(), &lb.end_iter(), false).to_string();
                let rt = rb.text(&rb.start_iter(), &rb.end_iter(), false).to_string();
                let patch = generate_unified_diff(
                    &format!("a/{}", ll.text()),
                    &format!("b/{}", rl.text()),
                    &lt,
                    &rt,
                    &ch.borrow(),
                );
                let dialog = gtk4::FileDialog::builder()
                    .title("Export Patch")
                    .initial_name("diff.patch")
                    .build();
                let win = pb
                    .root()
                    .and_then(|r| r.downcast::<ApplicationWindow>().ok());
                let dialog_ref = dialog.clone();
                dialog_ref.save(win.as_ref(), gio::Cancellable::NONE, move |result| {
                    if let Ok(file) = result
                        && let Some(path) = file.path()
                    {
                        let _ = fs::write(&path, &patch);
                    }
                });
            }
        };

        let export = Rc::new(export_patch);
        {
            let e = export.clone();
            patch_btn.connect_clicked(move |_| e());
        }
        {
            let action = gio::SimpleAction::new("export-patch", None);
            let e = export.clone();
            action.connect_activate(move |_, _| e());
            action_group.add_action(&action);
        }
    }

    DiffViewResult {
        widget,
        left_buf,
        right_buf,
        left_save: left_pane.save_btn,
        right_save: right_pane.save_btn,
        action_group,
    }
}

// ─── Open file diff in new tab ─────────────────────────────────────────────

fn open_file_diff(
    notebook: &Notebook,
    rel_path: &str,
    _status: &str,
    open_tabs: &Rc<RefCell<Vec<FileTab>>>,
    left_dir: &str,
    right_dir: &str,
    settings: &Rc<RefCell<Settings>>,
) {
    let left_path = Path::new(left_dir).join(rel_path);
    let right_path = Path::new(right_dir).join(rel_path);

    let dv = build_diff_view(&left_path, &right_path, &[], settings);
    dv.widget
        .insert_action_group("diff", Some(&dv.action_group));

    // Track tab
    let tab_id = NEXT_TAB_ID.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
    open_tabs.borrow_mut().push(FileTab {
        id: tab_id,
        rel_path: rel_path.to_string(),
        left_buf: dv.left_buf,
        right_buf: dv.right_buf,
        left_save: dv.left_save,
        right_save: dv.right_save,
    });

    // Tab label
    let file_name = Path::new(rel_path).file_name().map_or_else(
        || rel_path.to_string(),
        |n| n.to_string_lossy().into_owned(),
    );
    let left_dir_name = Path::new(left_dir)
        .file_name()
        .map(|n| n.to_string_lossy().into_owned())
        .unwrap_or_default();
    let right_dir_name = Path::new(right_dir)
        .file_name()
        .map(|n| n.to_string_lossy().into_owned())
        .unwrap_or_default();
    let tab_title = format!("[{left_dir_name}] {file_name} — [{right_dir_name}] {file_name}");

    let tab_label_box = GtkBox::new(Orientation::Horizontal, 4);
    let label = Label::new(Some(&tab_title));
    let close_btn = Button::from_icon_name("window-close-symbolic");
    close_btn.set_has_frame(false);
    tab_label_box.append(&label);
    tab_label_box.append(&close_btn);

    let page_num = notebook.append_page(&dv.widget, Some(&tab_label_box));
    notebook.set_current_page(Some(page_num));

    let nb = notebook.clone();
    let w = dv.widget.clone();
    let tabs = open_tabs.clone();
    close_btn.connect_clicked(move |_| {
        if let Some(n) = nb.page_num(&w) {
            nb.remove_page(Some(n));
        }
        tabs.borrow_mut().retain(|t| t.id != tab_id);
    });
}

// ─── 3-way merge view ───────────────────────────────────────────────────────

struct MergeViewResult {
    widget: GtkBox,
    left_buf: TextBuffer,
    middle_buf: TextBuffer,
    right_buf: TextBuffer,
    middle_save: Button,
    action_group: gio::SimpleActionGroup,
}

#[allow(clippy::too_many_arguments)]
fn refresh_merge_diffs(
    left_buf: &TextBuffer,
    middle_buf: &TextBuffer,
    right_buf: &TextBuffer,
    left_chunks: &Rc<RefCell<Vec<DiffChunk>>>,
    right_chunks: &Rc<RefCell<Vec<DiffChunk>>>,
    on_complete: impl Fn() + 'static,
    ignore_blanks: bool,
    ignore_whitespace: bool,
) {
    let lt = left_buf
        .text(&left_buf.start_iter(), &left_buf.end_iter(), false)
        .to_string();
    let mt = middle_buf
        .text(&middle_buf.start_iter(), &middle_buf.end_iter(), false)
        .to_string();
    let rt = right_buf
        .text(&right_buf.start_iter(), &right_buf.end_iter(), false)
        .to_string();

    remove_diff_tags(left_buf);
    remove_diff_tags(middle_buf);
    remove_diff_tags(right_buf);

    let lb = left_buf.clone();
    let mb = middle_buf.clone();
    let rb = right_buf.clone();
    let lch = left_chunks.clone();
    let rch = right_chunks.clone();

    gtk4::glib::spawn_future_local(async move {
        let (lt_cmp, lt_map) = filter_for_diff(&lt, ignore_whitespace, ignore_blanks);
        let (mt_cmp, mt_map) = filter_for_diff(&mt, ignore_whitespace, ignore_blanks);
        let (rt_cmp, rt_map) = filter_for_diff(&rt, ignore_whitespace, ignore_blanks);
        let lt_total = lt.lines().count();
        let mt_total = mt.lines().count();
        let rt_total = rt.lines().count();
        let left_identical = lt_cmp == mt_cmp;
        let right_identical = mt_cmp == rt_cmp;
        let (new_left, new_right) = gio::spawn_blocking(move || {
            let nl = if left_identical {
                Vec::new()
            } else {
                myers::diff_lines(&lt_cmp, &mt_cmp)
            };
            let nr = if right_identical {
                Vec::new()
            } else {
                myers::diff_lines(&mt_cmp, &rt_cmp)
            };
            (nl, nr)
        })
        .await
        .unwrap_or_default();

        let new_left = remap_chunks(new_left, &lt_map, lt_total, &mt_map, mt_total);
        let new_right = remap_chunks(new_right, &mt_map, mt_total, &rt_map, rt_total);

        apply_merge_tags(&lb, &mb, &rb, &new_left, &new_right);

        *lch.borrow_mut() = new_left;
        *rch.borrow_mut() = new_right;
        on_complete();
    });
}

fn setup_scroll_sync_3way(
    left_scroll: &ScrolledWindow,
    middle_scroll: &ScrolledWindow,
    right_scroll: &ScrolledWindow,
    left_gutter: &DrawingArea,
    right_gutter: &DrawingArea,
) {
    let syncing = Rc::new(Cell::new(false));

    // Left → Middle, Right
    {
        let ms = middle_scroll.clone();
        let rs = right_scroll.clone();
        let s = syncing.clone();
        let lg = left_gutter.clone();
        let rg = right_gutter.clone();
        left_scroll.vadjustment().connect_value_changed(move |adj| {
            if !s.get() {
                s.set(true);
                sync_adjustment(&ms.vadjustment(), adj);
                sync_adjustment(&rs.vadjustment(), adj);
                lg.queue_draw();
                rg.queue_draw();
                s.set(false);
            }
        });
    }

    // Middle → Left, Right
    {
        let ls = left_scroll.clone();
        let rs = right_scroll.clone();
        let s = syncing.clone();
        let lg = left_gutter.clone();
        let rg = right_gutter.clone();
        middle_scroll
            .vadjustment()
            .connect_value_changed(move |adj| {
                if !s.get() {
                    s.set(true);
                    sync_adjustment(&ls.vadjustment(), adj);
                    sync_adjustment(&rs.vadjustment(), adj);
                    lg.queue_draw();
                    rg.queue_draw();
                    s.set(false);
                }
            });
    }

    // Right → Left, Middle
    {
        let ls = left_scroll.clone();
        let ms = middle_scroll.clone();
        let s = syncing.clone();
        let lg = left_gutter.clone();
        let rg = right_gutter.clone();
        right_scroll
            .vadjustment()
            .connect_value_changed(move |adj| {
                if !s.get() {
                    s.set(true);
                    sync_adjustment(&ls.vadjustment(), adj);
                    sync_adjustment(&ms.vadjustment(), adj);
                    lg.queue_draw();
                    rg.queue_draw();
                    s.set(false);
                }
            });
    }
}

/// Collect non-Equal chunks from both diffs, sorted by middle-file line position.
/// Returns (`chunk_index`, `is_right_diff`) pairs.
fn merge_change_indices(
    left_chunks: &[DiffChunk],
    right_chunks: &[DiffChunk],
) -> Vec<(usize, bool)> {
    let mut indices: Vec<(usize, bool, usize)> = Vec::new();
    for (i, c) in left_chunks.iter().enumerate() {
        if c.tag != DiffTag::Equal {
            // In left diff, middle lines = start_b
            indices.push((i, false, c.start_b));
        }
    }
    for (i, c) in right_chunks.iter().enumerate() {
        if c.tag != DiffTag::Equal {
            // In right diff, middle lines = start_a
            indices.push((i, true, c.start_a));
        }
    }
    indices.sort_by_key(|&(_, _, line)| line);
    indices
        .into_iter()
        .map(|(i, is_right, _)| (i, is_right))
        .collect()
}

fn build_merge_view(
    left_path: &Path,
    middle_path: &Path,
    right_path: &Path,
    output: Option<&Path>,
    labels: &[String],
    settings: &Rc<RefCell<Settings>>,
) -> MergeViewResult {
    let s = settings.borrow();
    let (left_content, left_binary) = read_file_content(left_path);
    // When --output is set, the middle pane shows the merged file (with conflict markers),
    // not the base. This matches git mergetool semantics: you edit $MERGED, not $BASE.
    let middle_display_path = output.unwrap_or(middle_path);
    let (middle_content, middle_binary) = read_file_content(middle_display_path);
    let (right_content, right_binary) = read_file_content(right_path);
    let any_binary = left_binary || middle_binary || right_binary;

    let left_buf = create_source_buffer(left_path, &s);
    let middle_buf = create_source_buffer(middle_display_path, &s);
    let right_buf = create_source_buffer(right_path, &s);
    left_buf.set_text(&left_content);
    middle_buf.set_text(&middle_content);
    right_buf.set_text(&right_content);

    let left_identical = !any_binary && left_content == middle_content;
    let right_identical = !any_binary && middle_content == right_content;

    let left_chunks = Rc::new(RefCell::new(Vec::new()));
    let right_chunks = Rc::new(RefCell::new(Vec::new()));

    let left_pane = make_diff_pane(
        &left_buf,
        left_path,
        None,
        labels.first().map(String::as_str),
        &s,
    );
    let middle_pane = make_diff_pane(
        &middle_buf,
        middle_display_path,
        None,
        labels.get(1).map(String::as_str),
        &s,
    );
    let right_pane = make_diff_pane(
        &right_buf,
        right_path,
        None,
        labels.get(2).map(String::as_str),
        &s,
    );
    drop(s);

    // Track which text view was last focused
    let active_view: Rc<RefCell<TextView>> = Rc::new(RefCell::new(middle_pane.text_view.clone()));
    for tv in [
        &left_pane.text_view,
        &middle_pane.text_view,
        &right_pane.text_view,
    ] {
        let av = active_view.clone();
        let t = tv.clone();
        let fc = EventControllerFocus::new();
        fc.connect_enter(move |_| {
            *av.borrow_mut() = t.clone();
        });
        tv.add_controller(fc);
    }

    // Left gutter (left ↔ middle)
    let left_gutter = DrawingArea::new();
    left_gutter.set_content_width(48);
    left_gutter.set_vexpand(true);

    {
        let ltv = left_pane.text_view.clone();
        let mtv = middle_pane.text_view.clone();
        let lb = left_buf.clone();
        let mb = middle_buf.clone();
        let ls = left_pane.scroll.clone();
        let ms = middle_pane.scroll.clone();
        let ch = left_chunks.clone();
        left_gutter.set_draw_func(move |area, cr, width, _height| {
            draw_gutter(
                cr,
                width as f64,
                &ltv,
                &mtv,
                &lb,
                &mb,
                &ls,
                &ms,
                area,
                &ch.borrow(),
            );
        });
    }

    // Left gutter click: → copies left→middle, ← copies middle→left
    {
        let gesture = GestureClick::new();
        let ltv = left_pane.text_view.clone();
        let mtv = middle_pane.text_view.clone();
        let lb = left_buf.clone();
        let mb = middle_buf.clone();
        let ls = left_pane.scroll.clone();
        let ms = middle_pane.scroll.clone();
        let ch = left_chunks.clone();
        let g = left_gutter.clone();
        gesture.connect_pressed(move |_, _, x, y| {
            handle_gutter_click(
                x,
                y,
                g.width() as f64,
                &ltv,
                &mtv,
                &lb,
                &mb,
                &ls,
                &ms,
                &g,
                &ch,
            );
        });
        left_gutter.add_controller(gesture);
    }

    // Left gutter right-click context menu
    {
        let lg_pending: Rc<Cell<Option<usize>>> = Rc::new(Cell::new(None));
        let lg_ctx = gio::SimpleActionGroup::new();
        {
            let action = gio::SimpleAction::new("copy-left-middle", None);
            let pc = lg_pending.clone();
            let ch = left_chunks.clone();
            let lb = left_buf.clone();
            let mb = middle_buf.clone();
            action.connect_activate(move |_, _| {
                if let Some(idx) = pc.get() {
                    let s = ch.borrow();
                    if let Some(c) = s.get(idx) {
                        copy_chunk(&lb, c.start_a, c.end_a, &mb, c.start_b, c.end_b);
                    }
                }
            });
            lg_ctx.add_action(&action);
        }
        {
            let action = gio::SimpleAction::new("copy-middle-left", None);
            let pc = lg_pending.clone();
            let ch = left_chunks.clone();
            let lb = left_buf.clone();
            let mb = middle_buf.clone();
            action.connect_activate(move |_, _| {
                if let Some(idx) = pc.get() {
                    let s = ch.borrow();
                    if let Some(c) = s.get(idx) {
                        copy_chunk(&mb, c.start_b, c.end_b, &lb, c.start_a, c.end_a);
                    }
                }
            });
            lg_ctx.add_action(&action);
        }
        left_gutter.insert_action_group("lgutter", Some(&lg_ctx));

        let lg_menu = gio::Menu::new();
        lg_menu.append(
            Some("Copy Left \u{2192} Middle"),
            Some("lgutter.copy-left-middle"),
        );
        lg_menu.append(
            Some("Copy Middle \u{2192} Left"),
            Some("lgutter.copy-middle-left"),
        );
        let lg_popover = PopoverMenu::from_model(Some(&lg_menu));
        lg_popover.set_parent(&left_gutter);
        lg_popover.set_has_arrow(false);

        let gesture = GestureClick::new();
        gesture.set_button(3);
        let ltv = left_pane.text_view.clone();
        let mtv = middle_pane.text_view.clone();
        let lb = left_buf.clone();
        let mb = middle_buf.clone();
        let ls = left_pane.scroll.clone();
        let ms = middle_pane.scroll.clone();
        let ch = left_chunks.clone();
        let g = left_gutter.clone();
        let pc = lg_pending;
        let pop = lg_popover;
        gesture.connect_pressed(move |_, _, x, y| {
            let snapshot = ch.borrow();
            for (idx, chunk) in snapshot.iter().enumerate() {
                if chunk.tag == DiffTag::Equal {
                    continue;
                }
                let lt = line_to_gutter_y(&ltv, &lb, chunk.start_a, &ls, &g);
                let lb_y = line_to_gutter_y(&ltv, &lb, chunk.end_a, &ls, &g);
                let mt = line_to_gutter_y(&mtv, &mb, chunk.start_b, &ms, &g);
                let mb_y = line_to_gutter_y(&mtv, &mb, chunk.end_b, &ms, &g);
                let top = lt.min(mt) - 6.0;
                let bottom = lb_y.max(mb_y) + 6.0;
                if y >= top && y <= bottom {
                    pc.set(Some(idx));
                    pop.set_pointing_to(Some(&gtk4::gdk::Rectangle::new(x as i32, y as i32, 1, 1)));
                    pop.popup();
                    return;
                }
            }
        });
        left_gutter.add_controller(gesture);
    }

    // Right gutter (middle ↔ right)
    let right_gutter = DrawingArea::new();
    right_gutter.set_content_width(48);
    right_gutter.set_vexpand(true);

    {
        let mtv = middle_pane.text_view.clone();
        let rtv = right_pane.text_view.clone();
        let mb = middle_buf.clone();
        let rb = right_buf.clone();
        let ms = middle_pane.scroll.clone();
        let rs = right_pane.scroll.clone();
        let ch = right_chunks.clone();
        right_gutter.set_draw_func(move |area, cr, width, _height| {
            draw_gutter(
                cr,
                width as f64,
                &mtv,
                &rtv,
                &mb,
                &rb,
                &ms,
                &rs,
                area,
                &ch.borrow(),
            );
        });
    }

    // Right gutter click: → copies middle→right, ← copies right→middle
    {
        let gesture = GestureClick::new();
        let mtv = middle_pane.text_view.clone();
        let rtv = right_pane.text_view.clone();
        let mb = middle_buf.clone();
        let rb = right_buf.clone();
        let ms = middle_pane.scroll.clone();
        let rs = right_pane.scroll.clone();
        let ch = right_chunks.clone();
        let g = right_gutter.clone();
        gesture.connect_pressed(move |_, _, x, y| {
            handle_gutter_click(
                x,
                y,
                g.width() as f64,
                &mtv,
                &rtv,
                &mb,
                &rb,
                &ms,
                &rs,
                &g,
                &ch,
            );
        });
        right_gutter.add_controller(gesture);
    }

    // Right gutter right-click context menu
    {
        let rg_pending: Rc<Cell<Option<usize>>> = Rc::new(Cell::new(None));
        let rg_ctx = gio::SimpleActionGroup::new();
        {
            let action = gio::SimpleAction::new("copy-middle-right", None);
            let pc = rg_pending.clone();
            let ch = right_chunks.clone();
            let mb = middle_buf.clone();
            let rb = right_buf.clone();
            action.connect_activate(move |_, _| {
                if let Some(idx) = pc.get() {
                    let s = ch.borrow();
                    if let Some(c) = s.get(idx) {
                        copy_chunk(&mb, c.start_a, c.end_a, &rb, c.start_b, c.end_b);
                    }
                }
            });
            rg_ctx.add_action(&action);
        }
        {
            let action = gio::SimpleAction::new("copy-right-middle", None);
            let pc = rg_pending.clone();
            let ch = right_chunks.clone();
            let mb = middle_buf.clone();
            let rb = right_buf.clone();
            action.connect_activate(move |_, _| {
                if let Some(idx) = pc.get() {
                    let s = ch.borrow();
                    if let Some(c) = s.get(idx) {
                        copy_chunk(&rb, c.start_b, c.end_b, &mb, c.start_a, c.end_a);
                    }
                }
            });
            rg_ctx.add_action(&action);
        }
        right_gutter.insert_action_group("rgutter", Some(&rg_ctx));

        let rg_menu = gio::Menu::new();
        rg_menu.append(
            Some("Copy Middle \u{2192} Right"),
            Some("rgutter.copy-middle-right"),
        );
        rg_menu.append(
            Some("Copy Right \u{2192} Middle"),
            Some("rgutter.copy-right-middle"),
        );
        let rg_popover = PopoverMenu::from_model(Some(&rg_menu));
        rg_popover.set_parent(&right_gutter);
        rg_popover.set_has_arrow(false);

        let gesture = GestureClick::new();
        gesture.set_button(3);
        let mtv = middle_pane.text_view.clone();
        let rtv = right_pane.text_view.clone();
        let mb = middle_buf.clone();
        let rb = right_buf.clone();
        let ms = middle_pane.scroll.clone();
        let rs = right_pane.scroll.clone();
        let ch = right_chunks.clone();
        let g = right_gutter.clone();
        let pc = rg_pending;
        let pop = rg_popover;
        gesture.connect_pressed(move |_, _, x, y| {
            let snapshot = ch.borrow();
            for (idx, chunk) in snapshot.iter().enumerate() {
                if chunk.tag == DiffTag::Equal {
                    continue;
                }
                let mt = line_to_gutter_y(&mtv, &mb, chunk.start_a, &ms, &g);
                let mb_y = line_to_gutter_y(&mtv, &mb, chunk.end_a, &ms, &g);
                let rt = line_to_gutter_y(&rtv, &rb, chunk.start_b, &rs, &g);
                let rb_y = line_to_gutter_y(&rtv, &rb, chunk.end_b, &rs, &g);
                let top = mt.min(rt) - 6.0;
                let bottom = mb_y.max(rb_y) + 6.0;
                if y >= top && y <= bottom {
                    pc.set(Some(idx));
                    pop.set_pointing_to(Some(&gtk4::gdk::Rectangle::new(x as i32, y as i32, 1, 1)));
                    pop.popup();
                    return;
                }
            }
        });
        right_gutter.add_controller(gesture);
    }

    // Text filter state
    let ignore_blanks: Rc<Cell<bool>> = Rc::new(Cell::new(false));
    let ignore_whitespace: Rc<Cell<bool>> = Rc::new(Cell::new(false));

    // Scroll sync
    setup_scroll_sync_3way(
        &left_pane.scroll,
        &middle_pane.scroll,
        &right_pane.scroll,
        &left_gutter,
        &right_gutter,
    );

    // ── Toolbar with chunk navigation ───────────────────────────
    let current_chunk: Rc<Cell<Option<(usize, bool)>>> = Rc::new(Cell::new(None));

    let chunk_label = Label::new(None);
    chunk_label.add_css_class("chunk-label");
    {
        let total = count_changes(&left_chunks.borrow()) + count_changes(&right_chunks.borrow());
        if total == 0 {
            chunk_label.set_label("No changes");
        } else {
            chunk_label.set_label(&format!("{total} changes"));
        }
    }

    let prev_btn = Button::from_icon_name("go-up-symbolic");
    prev_btn.set_tooltip_text(Some("Previous change (Alt+Up / Ctrl+E)"));
    let next_btn = Button::from_icon_name("go-down-symbolic");
    next_btn.set_tooltip_text(Some("Next change (Alt+Down / Ctrl+D)"));

    let nav_box = GtkBox::new(Orientation::Horizontal, 0);
    nav_box.add_css_class("linked");
    nav_box.append(&prev_btn);
    nav_box.append(&next_btn);

    // Undo/Redo buttons
    let undo_btn = Button::from_icon_name("edit-undo-symbolic");
    undo_btn.set_tooltip_text(Some("Undo (Ctrl+Z)"));
    let redo_btn = Button::from_icon_name("edit-redo-symbolic");
    redo_btn.set_tooltip_text(Some("Redo (Ctrl+Shift+Z)"));
    let undo_redo_box = GtkBox::new(Orientation::Horizontal, 0);
    undo_redo_box.add_css_class("linked");
    undo_redo_box.append(&undo_btn);
    undo_redo_box.append(&redo_btn);
    {
        let av = active_view.clone();
        undo_btn.connect_clicked(move |_| {
            let buf = av.borrow().buffer();
            if buf.can_undo() {
                buf.undo();
            }
        });
    }
    {
        let av = active_view.clone();
        redo_btn.connect_clicked(move |_| {
            let buf = av.borrow().buffer();
            if buf.can_redo() {
                buf.redo();
            }
        });
    }

    // Go to line entry (hidden by default)
    let goto_entry = Entry::new();
    goto_entry.set_placeholder_text(Some("Line #"));
    goto_entry.set_width_chars(8);
    goto_entry.add_css_class("goto-entry");
    goto_entry.set_visible(false);
    {
        let av = active_view.clone();
        let ms = middle_pane.scroll.clone();
        let entry = goto_entry.clone();
        goto_entry.connect_activate(move |e| {
            if let Ok(line) = e.text().trim().parse::<usize>() {
                let tv = av.borrow().clone();
                let buf = tv.buffer();
                let target = line.saturating_sub(1);
                scroll_to_line(&tv, &buf, target, &ms);
                if let Some(iter) = buf.iter_at_line(target as i32) {
                    buf.place_cursor(&iter);
                }
            }
            e.set_visible(false);
            entry.set_text("");
        });
    }
    {
        let entry = goto_entry.clone();
        let key_ctl = EventControllerKey::new();
        key_ctl.connect_key_pressed(move |_, key, _, _| {
            if key == gtk4::gdk::Key::Escape {
                entry.set_visible(false);
                entry.set_text("");
                return gtk4::glib::Propagation::Stop;
            }
            gtk4::glib::Propagation::Proceed
        });
        goto_entry.add_controller(key_ctl);
    }

    let toolbar = GtkBox::new(Orientation::Horizontal, 8);
    toolbar.set_margin_start(6);
    toolbar.set_margin_end(6);
    toolbar.set_margin_top(4);
    toolbar.set_margin_bottom(4);
    // Text filter toggles
    let blank_toggle = ToggleButton::with_label("Blanks");
    blank_toggle.set_tooltip_text(Some("Ignore blank lines"));
    let ws_toggle = ToggleButton::with_label("Spaces");
    ws_toggle.set_tooltip_text(Some("Ignore whitespace differences"));
    let filter_box = GtkBox::new(Orientation::Horizontal, 0);
    filter_box.add_css_class("linked");
    filter_box.append(&blank_toggle);
    filter_box.append(&ws_toggle);

    let merge_prefs_btn = Button::from_icon_name("preferences-system-symbolic");
    merge_prefs_btn.set_tooltip_text(Some("Preferences (Ctrl+,)"));
    merge_prefs_btn.set_action_name(Some("win.prefs"));

    toolbar.append(&undo_redo_box);
    toolbar.append(&nav_box);
    toolbar.append(&chunk_label);
    toolbar.append(&goto_entry);
    toolbar.append(&filter_box);
    toolbar.append(&merge_prefs_btn);

    // Navigate helper for merge view
    let navigate_merge_chunk = |lch: &[DiffChunk],
                                rch: &[DiffChunk],
                                cur: &Rc<Cell<Option<(usize, bool)>>>,
                                direction: i32,
                                mtv: &TextView,
                                mb: &TextBuffer,
                                ms: &ScrolledWindow| {
        let all = merge_change_indices(lch, rch);
        if all.is_empty() {
            return;
        }
        let next = match cur.get() {
            Some(cur_val) => {
                let pos = all.iter().position(|v| *v == cur_val);
                match pos {
                    Some(p) => {
                        if direction > 0 {
                            all.get(p + 1).or(all.first())
                        } else if p > 0 {
                            all.get(p - 1)
                        } else {
                            all.last()
                        }
                    }
                    None => {
                        if direction > 0 {
                            all.first()
                        } else {
                            all.last()
                        }
                    }
                }
            }
            None => {
                if direction > 0 {
                    all.first()
                } else {
                    all.last()
                }
            }
        };
        if let Some(&(idx, is_right)) = next {
            cur.set(Some((idx, is_right)));
            // Scroll middle pane to the chunk
            let line = if is_right {
                rch[idx].start_a
            } else {
                lch[idx].start_b
            };
            scroll_to_line(mtv, mb, line, ms);
        }
    };

    let update_merge_label =
        |lbl: &Label, lch: &[DiffChunk], rch: &[DiffChunk], cur: Option<(usize, bool)>| {
            let all = merge_change_indices(lch, rch);
            let total = all.len();
            if total == 0 {
                lbl.set_label("No changes");
                return;
            }
            match cur {
                Some(cur_val) => {
                    if let Some(pos) = all.iter().position(|v| *v == cur_val) {
                        lbl.set_label(&format!("Change {} of {}", pos + 1, total));
                    } else {
                        lbl.set_label(&format!("{total} changes"));
                    }
                }
                None => lbl.set_label(&format!("{total} changes")),
            }
        };

    // Prev chunk
    {
        let lch = left_chunks.clone();
        let rch = right_chunks.clone();
        let cur = current_chunk.clone();
        let mtv = middle_pane.text_view.clone();
        let mb = middle_buf.clone();
        let ms = middle_pane.scroll.clone();
        let lbl = chunk_label.clone();
        let nav = navigate_merge_chunk;
        let upd = update_merge_label;
        prev_btn.connect_clicked(move |_| {
            nav(&lch.borrow(), &rch.borrow(), &cur, -1, &mtv, &mb, &ms);
            upd(&lbl, &lch.borrow(), &rch.borrow(), cur.get());
        });
    }

    // Next chunk
    {
        let lch = left_chunks.clone();
        let rch = right_chunks.clone();
        let cur = current_chunk.clone();
        let mtv = middle_pane.text_view.clone();
        let mb = middle_buf.clone();
        let ms = middle_pane.scroll.clone();
        let lbl = chunk_label.clone();
        let nav = navigate_merge_chunk;
        let upd = update_merge_label;
        next_btn.connect_clicked(move |_| {
            nav(&lch.borrow(), &rch.borrow(), &cur, 1, &mtv, &mb, &ms);
            upd(&lbl, &lch.borrow(), &rch.borrow(), cur.get());
        });
    }

    // ── Chunk maps for merge view ────────────────────────────────
    let left_chunk_map = DrawingArea::new();
    left_chunk_map.set_content_width(12);
    left_chunk_map.set_vexpand(true);
    {
        let lb = left_buf.clone();
        let ls = left_pane.scroll.clone();
        let ch = left_chunks.clone();
        left_chunk_map.set_draw_func(move |_area, cr, _w, h| {
            draw_chunk_map(cr, 12.0, h as f64, lb.line_count(), &ls, &ch.borrow(), true);
        });
    }
    {
        let gesture = GestureClick::new();
        let ls = left_pane.scroll.clone();
        let lm = left_chunk_map.clone();
        gesture.connect_pressed(move |_, _, _x, y| {
            let h = lm.height() as f64;
            if h > 0.0 {
                let adj = ls.vadjustment();
                let target = (y / h) * adj.upper() - adj.page_size() / 2.0;
                adj.set_value(target.max(0.0));
            }
        });
        left_chunk_map.add_controller(gesture);
    }

    let right_chunk_map = DrawingArea::new();
    right_chunk_map.set_content_width(12);
    right_chunk_map.set_vexpand(true);
    {
        let rb = right_buf.clone();
        let rs = right_pane.scroll.clone();
        let ch = right_chunks.clone();
        right_chunk_map.set_draw_func(move |_area, cr, _w, h| {
            draw_chunk_map(
                cr,
                12.0,
                h as f64,
                rb.line_count(),
                &rs,
                &ch.borrow(),
                false,
            );
        });
    }
    {
        let gesture = GestureClick::new();
        let rs = right_pane.scroll.clone();
        let rm = right_chunk_map.clone();
        gesture.connect_pressed(move |_, _, _x, y| {
            let h = rm.height() as f64;
            if h > 0.0 {
                let adj = rs.vadjustment();
                let target = (y / h) * adj.upper() - adj.page_size() / 2.0;
                adj.set_value(target.max(0.0));
            }
        });
        right_chunk_map.add_controller(gesture);
    }

    // Redraw chunk maps on scroll
    {
        let lcm = left_chunk_map.clone();
        let rcm = right_chunk_map.clone();
        middle_pane
            .scroll
            .vadjustment()
            .connect_value_changed(move |_| {
                lcm.queue_draw();
                rcm.queue_draw();
            });
    }

    // Re-diff on any buffer change, and update all visuals when diff completes
    {
        let make_on_complete = {
            let lg = left_gutter.clone();
            let rg = right_gutter.clone();
            let lcm = left_chunk_map.clone();
            let rcm = right_chunk_map.clone();
            let lbl = chunk_label.clone();
            let lch = left_chunks.clone();
            let rch = right_chunks.clone();
            let cur = current_chunk.clone();
            move || {
                let lg = lg.clone();
                let rg = rg.clone();
                let lcm = lcm.clone();
                let rcm = rcm.clone();
                let lbl = lbl.clone();
                let lch = lch.clone();
                let rch = rch.clone();
                let cur = cur.clone();
                move || {
                    lg.queue_draw();
                    rg.queue_draw();
                    lcm.queue_draw();
                    rcm.queue_draw();
                    cur.set(None);
                    update_merge_label(&lbl, &lch.borrow(), &rch.borrow(), None);
                }
            }
        };

        let pending = Rc::new(Cell::new(false));
        let connect_refresh = |buf: &TextBuffer| {
            let lb = left_buf.clone();
            let mb = middle_buf.clone();
            let rb = right_buf.clone();
            let lch = left_chunks.clone();
            let rch = right_chunks.clone();
            let p = pending.clone();
            let ib = ignore_blanks.clone();
            let iw = ignore_whitespace.clone();
            let make_cb = make_on_complete.clone();
            buf.connect_changed(move |_| {
                if !p.get() {
                    p.set(true);
                    let lb = lb.clone();
                    let mb = mb.clone();
                    let rb = rb.clone();
                    let lch = lch.clone();
                    let rch = rch.clone();
                    let p = p.clone();
                    let ib = ib.clone();
                    let iw = iw.clone();
                    let cb = make_cb();
                    gtk4::glib::idle_add_local_once(move || {
                        refresh_merge_diffs(&lb, &mb, &rb, &lch, &rch, cb, ib.get(), iw.get());
                        p.set(false);
                    });
                }
            });
        };
        connect_refresh(&left_buf);
        connect_refresh(&middle_buf);
        connect_refresh(&right_buf);

        // Initial async diff (must be after chunk_label + chunk_maps exist)
        if !any_binary && (!left_identical || !right_identical) {
            let lb = left_buf.clone();
            let mb = middle_buf.clone();
            let rb = right_buf.clone();
            let lch = left_chunks.clone();
            let rch = right_chunks.clone();
            let on_complete = make_on_complete();
            gtk4::glib::spawn_future_local(async move {
                let (new_left, new_right) = gio::spawn_blocking(move || {
                    let nl = if left_identical {
                        Vec::new()
                    } else {
                        myers::diff_lines(&left_content, &middle_content)
                    };
                    let nr = if right_identical {
                        Vec::new()
                    } else {
                        myers::diff_lines(&middle_content, &right_content)
                    };
                    (nl, nr)
                })
                .await
                .unwrap_or_default();

                apply_merge_tags(&lb, &mb, &rb, &new_left, &new_right);
                *lch.borrow_mut() = new_left;
                *rch.borrow_mut() = new_right;
                on_complete();
            });
        }

        // Toggle handlers for filter buttons
        {
            let ib = ignore_blanks.clone();
            let iw = ignore_whitespace.clone();
            let lb = left_buf.clone();
            let mb = middle_buf.clone();
            let rb = right_buf.clone();
            let lch = left_chunks.clone();
            let rch = right_chunks.clone();
            let make_cb = make_on_complete.clone();
            blank_toggle.connect_toggled(move |btn| {
                ib.set(btn.is_active());
                refresh_merge_diffs(&lb, &mb, &rb, &lch, &rch, make_cb(), ib.get(), iw.get());
            });
        }
        {
            let ib = ignore_blanks.clone();
            let iw = ignore_whitespace.clone();
            let lb = left_buf.clone();
            let mb = middle_buf.clone();
            let rb = right_buf.clone();
            let lch = left_chunks.clone();
            let rch = right_chunks.clone();
            ws_toggle.connect_toggled(move |btn| {
                iw.set(btn.is_active());
                refresh_merge_diffs(
                    &lb,
                    &mb,
                    &rb,
                    &lch,
                    &rch,
                    make_on_complete(),
                    ib.get(),
                    iw.get(),
                );
            });
        }
    }

    // ── Find bar for merge view ───────────────────────────────────
    let find_entry = Entry::new();
    find_entry.set_placeholder_text(Some("Find (Ctrl+F)"));
    find_entry.set_hexpand(true);

    let replace_entry = Entry::new();
    replace_entry.set_placeholder_text(Some("Replace"));
    replace_entry.set_hexpand(true);

    let find_prev_btn = Button::from_icon_name("go-up-symbolic");
    let find_next_btn = Button::from_icon_name("go-down-symbolic");
    let match_label = Label::new(None);
    match_label.add_css_class("chunk-label");
    let find_close_btn = Button::from_icon_name("window-close-symbolic");
    find_close_btn.set_has_frame(false);

    let replace_btn = Button::with_label("Replace");
    let replace_all_btn = Button::with_label("All");
    let replace_row = GtkBox::new(Orientation::Horizontal, 4);
    replace_row.set_margin_start(6);
    replace_row.set_margin_end(6);
    replace_row.append(&replace_entry);
    replace_row.append(&replace_btn);
    replace_row.append(&replace_all_btn);
    replace_row.set_visible(false);

    let find_nav = GtkBox::new(Orientation::Horizontal, 0);
    find_nav.add_css_class("linked");
    find_nav.append(&find_prev_btn);
    find_nav.append(&find_next_btn);

    let find_row = GtkBox::new(Orientation::Horizontal, 4);
    find_row.set_margin_start(6);
    find_row.set_margin_end(6);
    find_row.append(&find_entry);
    find_row.append(&find_nav);
    find_row.append(&match_label);
    find_row.append(&find_close_btn);

    let find_bar = GtkBox::new(Orientation::Vertical, 2);
    find_bar.add_css_class("find-bar");
    find_bar.append(&find_row);
    find_bar.append(&replace_row);

    let find_revealer = Revealer::new();
    find_revealer.set_child(Some(&find_bar));
    find_revealer.set_reveal_child(false);
    find_revealer.set_transition_type(gtk4::RevealerTransitionType::SlideUp);

    // Search logic
    {
        let lb = left_buf.clone();
        let mb = middle_buf.clone();
        let rb = right_buf.clone();
        let ml = match_label.clone();
        find_entry.connect_changed(move |e| {
            let needle = e.text().to_string();
            let total = highlight_search_matches(&lb, &needle)
                + highlight_search_matches(&mb, &needle)
                + highlight_search_matches(&rb, &needle);
            if needle.is_empty() {
                ml.set_label("");
            } else if total == 0 {
                ml.set_label("No matches");
            } else {
                ml.set_label(&format!("{total} matches"));
            }
        });
    }

    // Find next
    {
        let av = active_view.clone();
        let fe = find_entry.clone();
        let ms = middle_pane.scroll.clone();
        find_next_btn.connect_clicked(move |_| {
            let needle = fe.text().to_string();
            let tv = av.borrow().clone();
            let buf = tv.buffer();
            let cursor = buf.iter_at_mark(&buf.get_insert());
            if let Some((start, end)) = find_next_match(&buf, &needle, &cursor, true) {
                buf.select_range(&start, &end);
                scroll_to_line(&tv, &buf, start.line() as usize, &ms);
            }
        });
    }

    // Find prev
    {
        let av = active_view.clone();
        let fe = find_entry.clone();
        let ms = middle_pane.scroll.clone();
        find_prev_btn.connect_clicked(move |_| {
            let needle = fe.text().to_string();
            let tv = av.borrow().clone();
            let buf = tv.buffer();
            let cursor = buf.iter_at_mark(&buf.get_insert());
            if let Some((start, end)) = find_next_match(&buf, &needle, &cursor, false) {
                buf.select_range(&start, &end);
                scroll_to_line(&tv, &buf, start.line() as usize, &ms);
            }
        });
    }

    // Enter in find entry = find next
    {
        let av = active_view.clone();
        let ms = middle_pane.scroll.clone();
        find_entry.connect_activate(move |e| {
            let needle = e.text().to_string();
            let tv = av.borrow().clone();
            let buf = tv.buffer();
            let cursor = buf.iter_at_mark(&buf.get_insert());
            if let Some((start, end)) = find_next_match(&buf, &needle, &cursor, true) {
                buf.select_range(&start, &end);
                scroll_to_line(&tv, &buf, start.line() as usize, &ms);
            }
        });
    }

    // Replace
    {
        let av = active_view.clone();
        let find_e = find_entry.clone();
        let repl_e = replace_entry.clone();
        replace_btn.connect_clicked(move |_| {
            let tv = av.borrow().clone();
            let buf = tv.buffer();
            let (sel_start, sel_end) = buf.selection_bounds().unwrap_or_else(|| {
                let c = buf.iter_at_mark(&buf.get_insert());
                (c, c)
            });
            let selected = buf.text(&sel_start, &sel_end, false).to_string();
            let needle = find_e.text().to_string();
            if !needle.is_empty() && selected.to_lowercase() == needle.to_lowercase() {
                let replacement = repl_e.text().to_string();
                let mut s = sel_start;
                let mut e = sel_end;
                buf.delete(&mut s, &mut e);
                buf.insert(&mut s, &replacement);
            }
        });
    }

    // Replace all
    {
        let lb = left_buf.clone();
        let mb = middle_buf.clone();
        let rb = right_buf.clone();
        let find_e = find_entry.clone();
        let repl_e = replace_entry.clone();
        replace_all_btn.connect_clicked(move |_| {
            let needle = find_e.text().to_string();
            let replacement = repl_e.text().to_string();
            if needle.is_empty() {
                return;
            }
            for buf in [&lb, &mb, &rb] {
                let text = buf
                    .text(&buf.start_iter(), &buf.end_iter(), false)
                    .to_string();
                let new_text = text.replace(&needle, &replacement);
                if new_text != text {
                    buf.set_text(&new_text);
                }
            }
        });
    }

    // Close find bar
    {
        let fr = find_revealer.clone();
        let lb = left_buf.clone();
        let mb = middle_buf.clone();
        let rb = right_buf.clone();
        find_close_btn.connect_clicked(move |_| {
            fr.set_reveal_child(false);
            clear_search_tags(&lb);
            clear_search_tags(&mb);
            clear_search_tags(&rb);
        });
    }

    // Escape in find entry
    {
        let fr = find_revealer.clone();
        let lb = left_buf.clone();
        let mb = middle_buf.clone();
        let rb = right_buf.clone();
        let key_ctl = EventControllerKey::new();
        key_ctl.connect_key_pressed(move |_, key, _, _| {
            if key == gtk4::gdk::Key::Escape {
                fr.set_reveal_child(false);
                clear_search_tags(&lb);
                clear_search_tags(&mb);
                clear_search_tags(&rb);
                return gtk4::glib::Propagation::Stop;
            }
            gtk4::glib::Propagation::Proceed
        });
        find_entry.add_controller(key_ctl);
    }

    // Layout: [chunk_map | left pane | left_gutter | middle pane | right_gutter | right pane | chunk_map]
    let diff_row = GtkBox::new(Orientation::Horizontal, 0);
    left_pane.container.set_hexpand(true);
    middle_pane.container.set_hexpand(true);
    right_pane.container.set_hexpand(true);
    diff_row.append(&left_chunk_map);
    diff_row.append(&left_pane.container);
    diff_row.append(&left_gutter);
    diff_row.append(&middle_pane.container);
    diff_row.append(&right_gutter);
    diff_row.append(&right_pane.container);
    diff_row.append(&right_chunk_map);
    diff_row.set_vexpand(true);

    let widget = GtkBox::new(Orientation::Vertical, 0);
    widget.append(&toolbar);
    widget.append(&gtk4::Separator::new(Orientation::Horizontal));
    widget.append(&diff_row);
    widget.append(&find_revealer);

    // GAction group for keyboard shortcuts
    let action_group = gio::SimpleActionGroup::new();
    {
        let action = gio::SimpleAction::new("prev-chunk", None);
        let lch = left_chunks.clone();
        let rch = right_chunks.clone();
        let cur = current_chunk.clone();
        let mtv = middle_pane.text_view.clone();
        let mb = middle_buf.clone();
        let ms = middle_pane.scroll.clone();
        let lbl = chunk_label.clone();
        action.connect_activate(move |_, _| {
            navigate_merge_chunk(&lch.borrow(), &rch.borrow(), &cur, -1, &mtv, &mb, &ms);
            update_merge_label(&lbl, &lch.borrow(), &rch.borrow(), cur.get());
        });
        action_group.add_action(&action);
    }
    {
        let action = gio::SimpleAction::new("next-chunk", None);
        let lch = left_chunks.clone();
        let rch = right_chunks.clone();
        let cur = current_chunk.clone();
        let mtv = middle_pane.text_view.clone();
        let mb = middle_buf.clone();
        let ms = middle_pane.scroll.clone();
        let lbl = chunk_label.clone();
        action.connect_activate(move |_, _| {
            navigate_merge_chunk(&lch.borrow(), &rch.borrow(), &cur, 1, &mtv, &mb, &ms);
            update_merge_label(&lbl, &lch.borrow(), &rch.borrow(), cur.get());
        });
        action_group.add_action(&action);
    }
    // Find action (Ctrl+F)
    {
        let action = gio::SimpleAction::new("find", None);
        let fr = find_revealer.clone();
        let fe = find_entry.clone();
        let rr = replace_row.clone();
        action.connect_activate(move |_, _| {
            rr.set_visible(false);
            fr.set_reveal_child(true);
            fe.grab_focus();
        });
        action_group.add_action(&action);
    }
    // Find-replace action (Ctrl+H)
    {
        let action = gio::SimpleAction::new("find-replace", None);
        let fr = find_revealer.clone();
        let fe = find_entry.clone();
        let rr = replace_row.clone();
        action.connect_activate(move |_, _| {
            rr.set_visible(true);
            fr.set_reveal_child(true);
            fe.grab_focus();
        });
        action_group.add_action(&action);
    }
    // Find next (F3)
    {
        let action = gio::SimpleAction::new("find-next", None);
        let av = active_view.clone();
        let fe = find_entry.clone();
        let ms = middle_pane.scroll.clone();
        action.connect_activate(move |_, _| {
            let needle = fe.text().to_string();
            let tv = av.borrow().clone();
            let buf = tv.buffer();
            let cursor = buf.iter_at_mark(&buf.get_insert());
            if let Some((start, end)) = find_next_match(&buf, &needle, &cursor, true) {
                buf.select_range(&start, &end);
                scroll_to_line(&tv, &buf, start.line() as usize, &ms);
            }
        });
        action_group.add_action(&action);
    }
    // Find prev (Shift+F3)
    {
        let action = gio::SimpleAction::new("find-prev", None);
        let av = active_view.clone();
        let fe = find_entry.clone();
        let ms = middle_pane.scroll.clone();
        action.connect_activate(move |_, _| {
            let needle = fe.text().to_string();
            let tv = av.borrow().clone();
            let buf = tv.buffer();
            let cursor = buf.iter_at_mark(&buf.get_insert());
            if let Some((start, end)) = find_next_match(&buf, &needle, &cursor, false) {
                buf.select_range(&start, &end);
                scroll_to_line(&tv, &buf, start.line() as usize, &ms);
            }
        });
        action_group.add_action(&action);
    }
    // Go to line (Ctrl+L)
    {
        let action = gio::SimpleAction::new("go-to-line", None);
        let ge = goto_entry.clone();
        action.connect_activate(move |_, _| {
            ge.set_visible(true);
            ge.grab_focus();
        });
        action_group.add_action(&action);
    }

    MergeViewResult {
        widget,
        left_buf,
        middle_buf,
        right_buf,
        middle_save: middle_pane.save_btn,
        action_group,
    }
}

fn build_merge_window(
    app: &Application,
    left_path: std::path::PathBuf,
    middle_path: std::path::PathBuf,
    right_path: std::path::PathBuf,
    output: Option<std::path::PathBuf>,
    labels: &[String],
    settings: &Rc<RefCell<Settings>>,
) {
    let mv = build_merge_view(
        &left_path,
        &middle_path,
        &right_path,
        output.as_deref(),
        labels,
        settings,
    );

    // "Save Merged" button when --output is set
    if let Some(ref out_path) = output {
        let save_btn = Button::with_label("Save Merged");
        save_btn.set_tooltip_text(Some(&format!("Save to {}", out_path.display())));
        let mb = mv.middle_buf.clone();
        let op = out_path.clone();
        let btn_ref = save_btn.clone();
        save_btn.connect_clicked(move |_| {
            SAVING.store(true, std::sync::atomic::Ordering::Relaxed);
            let text = mb.text(&mb.start_iter(), &mb.end_iter(), false);
            let _ = fs::write(&op, text.as_str());
            btn_ref.set_sensitive(false);
            gtk4::glib::timeout_add_local_once(Duration::from_millis(600), move || {
                SAVING.store(false, std::sync::atomic::Ordering::Relaxed);
            });
        });
        // Re-enable after edits
        {
            let btn = save_btn.clone();
            mv.middle_buf.connect_changed(move |_| {
                btn.set_sensitive(true);
            });
        }
        // Insert the save button into toolbar (first child of the widget is the toolbar)
        if let Some(toolbar) = mv.widget.first_child()
            && let Some(toolbar_box) = toolbar.downcast_ref::<GtkBox>()
        {
            toolbar_box.append(&save_btn);
        }
    }

    // File watcher on all 3 files
    let (fs_tx, fs_rx) = mpsc::channel::<()>();
    {
        let lp = left_path.clone();
        let rp = right_path.clone();
        let op = output.clone().unwrap_or_else(|| middle_path.clone());
        std::thread::spawn(move || {
            use notify::{RecursiveMode, Watcher};
            let _watcher = {
                let mut w = notify::recommended_watcher(
                    move |res: Result<notify::Event, notify::Error>| {
                        if res.is_ok() {
                            let _ = fs_tx.send(());
                        }
                    },
                )
                .expect("Failed to create file watcher");
                for p in [&lp, &op, &rp] {
                    if let Some(parent) = p.parent() {
                        w.watch(parent, RecursiveMode::NonRecursive).ok();
                    }
                }
                w
            };
            loop {
                std::thread::park();
            }
        });
    }

    // Poll for filesystem changes and reload
    {
        let lb = mv.left_buf.clone();
        let mb = mv.middle_buf.clone();
        let rb = mv.right_buf.clone();
        let lp = left_path.clone();
        let mp = output.clone().unwrap_or_else(|| middle_path.clone());
        let rp = right_path.clone();
        let m_save = mv.middle_save.clone();
        gtk4::glib::timeout_add_local(Duration::from_millis(500), move || {
            let mut changed = false;
            while fs_rx.try_recv().is_ok() {
                changed = true;
            }
            if changed
                && !SAVING.load(std::sync::atomic::Ordering::Relaxed)
                && !m_save.is_sensitive()
            {
                let left_content = fs::read_to_string(&lp).unwrap_or_default();
                let middle_content = fs::read_to_string(&mp).unwrap_or_default();
                let right_content = fs::read_to_string(&rp).unwrap_or_default();

                let cur_l = lb.text(&lb.start_iter(), &lb.end_iter(), false);
                let cur_m = mb.text(&mb.start_iter(), &mb.end_iter(), false);
                let cur_r = rb.text(&rb.start_iter(), &rb.end_iter(), false);

                if cur_l.as_str() != left_content
                    || cur_m.as_str() != middle_content
                    || cur_r.as_str() != right_content
                {
                    // set_text triggers connect_changed which schedules
                    // refresh_merge_diffs with the current filter state.
                    lb.set_text(&left_content);
                    mb.set_text(&middle_content);
                    rb.set_text(&right_content);
                    m_save.set_sensitive(false);
                }
            }
            gtk4::glib::ControlFlow::Continue
        });
    }

    // Window title
    let left_name = left_path.file_name().map_or_else(
        || left_path.display().to_string(),
        |n| n.to_string_lossy().into_owned(),
    );
    let middle_name = middle_path.file_name().map_or_else(
        || middle_path.display().to_string(),
        |n| n.to_string_lossy().into_owned(),
    );
    let right_name = right_path.file_name().map_or_else(
        || right_path.display().to_string(),
        |n| n.to_string_lossy().into_owned(),
    );
    let title = format!("{left_name} — {middle_name} — {right_name}");

    let window = ApplicationWindow::builder()
        .application(app)
        .title(&title)
        .default_width(1200)
        .default_height(600)
        .child(&mv.widget)
        .build();
    window.insert_action_group("diff", Some(&mv.action_group));

    // Preferences action
    let win_actions = gio::SimpleActionGroup::new();
    {
        let action = gio::SimpleAction::new("prefs", None);
        let w = window.clone();
        let st = settings.clone();
        action.connect_activate(move |_, _| {
            show_preferences(&w, &st);
        });
        win_actions.add_action(&action);
    }
    window.insert_action_group("win", Some(&win_actions));

    if let Some(gtk_app) = window.application() {
        gtk_app.set_accels_for_action("diff.prev-chunk", &["<Alt>Up", "<Ctrl>e"]);
        gtk_app.set_accels_for_action("diff.next-chunk", &["<Alt>Down", "<Ctrl>d"]);
        gtk_app.set_accels_for_action("diff.find", &["<Ctrl>f"]);
        gtk_app.set_accels_for_action("diff.find-replace", &["<Ctrl>h"]);
        gtk_app.set_accels_for_action("diff.find-next", &["F3"]);
        gtk_app.set_accels_for_action("diff.find-prev", &["<Shift>F3"]);
        gtk_app.set_accels_for_action("diff.go-to-line", &["<Ctrl>l"]);
        gtk_app.set_accels_for_action("diff.export-patch", &["<Ctrl><Shift>p"]);
        gtk_app.set_accels_for_action("win.prefs", &["<Ctrl>comma"]);
    }

    window.present();
}

// ─── Preferences dialog ────────────────────────────────────────────────────

fn apply_settings_to_views(window: &gtk4::Window, settings: &Settings) {
    // Walk all sourceview5::View widgets in the window and re-apply settings
    fn walk(widget: &gtk4::Widget, settings: &Settings) {
        if widget.is::<sourceview5::View>() {
            let sv: sourceview5::View = widget.clone().downcast().unwrap();
            sv.set_show_line_numbers(settings.show_line_numbers);
            sv.set_highlight_current_line(settings.highlight_current_line);
            sv.set_wrap_mode(settings.wrap_mode_gtk());
            sv.set_tab_width(settings.tab_width);
            let buf: TextBuffer = sv.buffer();
            if let Ok(sbuf) = buf.downcast::<sourceview5::Buffer>() {
                let scheme_mgr = sourceview5::StyleSchemeManager::default();
                if let Some(scheme) = scheme_mgr.scheme(&settings.style_scheme) {
                    sbuf.set_style_scheme(Some(&scheme));
                }
            }
        }
        // Recurse into children
        let mut child = widget.first_child();
        while let Some(c) = child {
            walk(&c, settings);
            child = c.next_sibling();
        }
    }
    let w: gtk4::Widget = window.clone().upcast();
    walk(&w, settings);

    update_font_css(settings);
}

fn make_pref_row(label_text: &str, widget: &impl IsA<gtk4::Widget>) -> GtkBox {
    let row = GtkBox::new(Orientation::Horizontal, 12);
    row.set_margin_start(12);
    row.set_margin_end(12);
    row.set_margin_top(4);
    row.set_margin_bottom(4);
    let label = Label::new(Some(label_text));
    label.set_halign(gtk4::Align::Start);
    label.set_hexpand(true);
    row.append(&label);
    row.append(widget);
    row
}

fn show_preferences(parent: &ApplicationWindow, settings: &Rc<RefCell<Settings>>) {
    let win = gtk4::Window::builder()
        .title("Preferences")
        .transient_for(parent)
        .modal(true)
        .default_width(500)
        .default_height(450)
        .build();

    let content = GtkBox::new(Orientation::Vertical, 0);
    content.set_margin_top(8);
    content.set_margin_bottom(8);

    // ── Editor section header ──
    let editor_header = Label::new(Some("Editor"));
    editor_header.set_halign(gtk4::Align::Start);
    editor_header.set_margin_start(12);
    editor_header.set_margin_top(8);
    editor_header.set_margin_bottom(4);
    editor_header.add_css_class("heading");
    content.append(&editor_header);

    let s = settings.borrow();

    // Font
    let font_desc = gtk4::pango::FontDescription::from_string(&s.font);
    let font_dialog = gtk4::FontDialog::new();
    let font_btn = gtk4::FontDialogButton::new(Some(font_dialog));
    font_btn.set_font_desc(&font_desc);
    content.append(&make_pref_row("Font", &font_btn));

    // Style scheme
    let scheme_mgr = sourceview5::StyleSchemeManager::default();
    let scheme_ids = scheme_mgr.scheme_ids();
    let scheme_strings: Vec<String> = scheme_ids
        .iter()
        .map(std::string::ToString::to_string)
        .collect();
    let scheme_strs: Vec<&str> = scheme_strings.iter().map(String::as_str).collect();
    let scheme_list = gtk4::StringList::new(&scheme_strs);
    let scheme_dropdown = gtk4::DropDown::new(Some(scheme_list), gtk4::Expression::NONE);
    if let Some(pos) = scheme_strings.iter().position(|id| id == &s.style_scheme) {
        scheme_dropdown.set_selected(pos as u32);
    }
    content.append(&make_pref_row("Color scheme", &scheme_dropdown));

    // Show line numbers
    let line_num_switch = gtk4::Switch::new();
    line_num_switch.set_active(s.show_line_numbers);
    line_num_switch.set_valign(gtk4::Align::Center);
    content.append(&make_pref_row("Show line numbers", &line_num_switch));

    // Highlight current line
    let highlight_switch = gtk4::Switch::new();
    highlight_switch.set_active(s.highlight_current_line);
    highlight_switch.set_valign(gtk4::Align::Center);
    content.append(&make_pref_row("Highlight current line", &highlight_switch));

    // Word wrap
    let wrap_modes = gtk4::StringList::new(&["None", "Word", "Character"]);
    let wrap_dropdown = gtk4::DropDown::new(Some(wrap_modes), gtk4::Expression::NONE);
    wrap_dropdown.set_selected(match s.wrap_mode.as_str() {
        "word" => 1,
        "char" => 2,
        _ => 0,
    });
    content.append(&make_pref_row("Word wrap", &wrap_dropdown));

    // Tab width
    let tab_adj = Adjustment::new(f64::from(s.tab_width), 1.0, 16.0, 1.0, 4.0, 0.0);
    let tab_spin = gtk4::SpinButton::new(Some(&tab_adj), 1.0, 0);
    content.append(&make_pref_row("Tab width", &tab_spin));

    // ── File Filters section ──
    let filter_header = Label::new(Some("File Filters"));
    filter_header.set_halign(gtk4::Align::Start);
    filter_header.set_margin_start(12);
    filter_header.set_margin_top(16);
    filter_header.set_margin_bottom(4);
    filter_header.add_css_class("heading");
    content.append(&filter_header);

    let filter_desc = Label::new(Some("Names to exclude from directory comparison"));
    filter_desc.set_halign(gtk4::Align::Start);
    filter_desc.set_margin_start(12);
    filter_desc.add_css_class("dim-label");
    content.append(&filter_desc);

    let filter_list_box = GtkBox::new(Orientation::Vertical, 2);
    filter_list_box.set_margin_top(4);
    filter_list_box.set_margin_bottom(4);
    filter_list_box.set_margin_start(4);
    filter_list_box.set_margin_end(4);

    let filter_entries: Rc<RefCell<Vec<Entry>>> = Rc::new(RefCell::new(Vec::new()));

    let add_filter_entry = {
        let fe = filter_entries.clone();
        let flb = filter_list_box.clone();
        move |text: &str| {
            let row = GtkBox::new(Orientation::Horizontal, 4);
            let entry = Entry::new();
            entry.set_text(text);
            entry.set_hexpand(true);
            let remove_btn = Button::from_icon_name("list-remove-symbolic");
            remove_btn.set_tooltip_text(Some("Remove"));
            row.append(&entry);
            row.append(&remove_btn);
            flb.append(&row);
            fe.borrow_mut().push(entry.clone());

            let fe2 = fe.clone();
            let row_ref = row.clone();
            let flb2 = flb.clone();
            remove_btn.connect_clicked(move |_| {
                fe2.borrow_mut().retain(|e| e != &entry);
                flb2.remove(&row_ref);
            });
        }
    };

    for f in &s.dir_filters {
        add_filter_entry(f);
    }
    drop(s);

    let add_btn = Button::from_icon_name("list-add-symbolic");
    add_btn.set_tooltip_text(Some("Add filter"));
    add_btn.set_halign(gtk4::Align::Start);
    add_btn.set_margin_start(4);
    add_btn.set_margin_bottom(4);

    let filter_inner = GtkBox::new(Orientation::Vertical, 0);
    filter_inner.append(&filter_list_box);
    filter_inner.append(&add_btn);

    let filter_frame = gtk4::Frame::new(None);
    filter_frame.set_margin_start(12);
    filter_frame.set_margin_end(12);
    filter_frame.set_margin_top(4);
    filter_frame.set_child(Some(&filter_inner));
    content.append(&filter_frame);

    {
        let afe = add_filter_entry.clone();
        add_btn.connect_clicked(move |_| afe(""));
    }

    // ── Live-apply: shared closure to read controls, save, and apply ──
    let apply: Rc<dyn Fn()> = {
        let st = settings.clone();
        let p = parent.clone();
        let fb = font_btn.clone();
        let sd = scheme_dropdown.clone();
        let ss = scheme_strings.clone();
        let lns = line_num_switch.clone();
        let hls = highlight_switch.clone();
        let wd = wrap_dropdown.clone();
        let ts = tab_spin.clone();
        let fe = filter_entries.clone();
        Rc::new(move || {
            let mut s = st.borrow_mut();
            if let Some(fd) = fb.font_desc() {
                s.font = fd.to_string();
            }
            let idx = sd.selected() as usize;
            if idx < ss.len() {
                s.style_scheme.clone_from(&ss[idx]);
            }
            s.show_line_numbers = lns.is_active();
            s.highlight_current_line = hls.is_active();
            s.wrap_mode = match wd.selected() {
                1 => "word".into(),
                2 => "char".into(),
                _ => "none".into(),
            };
            s.tab_width = ts.value() as u32;
            s.dir_filters = fe
                .borrow()
                .iter()
                .map(|e| e.text().to_string())
                .filter(|t| !t.is_empty())
                .collect();
            s.save();
            apply_settings_to_views(p.upcast_ref(), &s);
        })
    };

    // Connect change signals for live-apply
    {
        let a = apply.clone();
        font_btn.connect_font_desc_notify(move |_| a());
    }
    {
        let a = apply.clone();
        scheme_dropdown.connect_selected_notify(move |_| a());
    }
    {
        let a = apply.clone();
        line_num_switch.connect_active_notify(move |_| a());
    }
    {
        let a = apply.clone();
        highlight_switch.connect_active_notify(move |_| a());
    }
    {
        let a = apply.clone();
        wrap_dropdown.connect_selected_notify(move |_| a());
    }
    {
        let a = apply.clone();
        tab_spin.connect_value_changed(move |_| a());
    }

    // Also save filters on close (they don't need live-apply)
    {
        let a = apply.clone();
        win.connect_close_request(move |_| {
            a();
            gtk4::glib::Propagation::Proceed
        });
    }

    let scroll = ScrolledWindow::builder()
        .hscrollbar_policy(PolicyType::Never)
        .child(&content)
        .build();
    win.set_child(Some(&scroll));
    win.present();
}

// ─── VCS window ─────────────────────────────────────────────────────────────

fn encode_vcs_row(status: &crate::vcs::VcsStatus, rel_path: &str) -> String {
    let code = match status {
        crate::vcs::VcsStatus::Modified => "M",
        crate::vcs::VcsStatus::Added => "A",
        crate::vcs::VcsStatus::Deleted => "D",
        crate::vcs::VcsStatus::Renamed => "R",
        crate::vcs::VcsStatus::Untracked => "U",
    };
    format!("{code}{SEP}{rel_path}")
}

fn decode_vcs_code(raw: &str) -> &str {
    raw.split(SEP).next().unwrap_or("")
}

fn decode_vcs_path(raw: &str) -> &str {
    raw.split_once(SEP).map_or("", |x| x.1)
}

fn vcs_status_label(code: &str) -> &str {
    match code {
        "M" => "Modified",
        "A" => "Added",
        "D" => "Deleted",
        "R" => "Renamed",
        "U" => "Untracked",
        _ => "",
    }
}

fn vcs_status_css(code: &str) -> &str {
    match code {
        "M" | "R" => "diff-changed",
        "A" | "U" => "diff-inserted",
        "D" => "diff-deleted",
        _ => "",
    }
}

fn make_vcs_status_factory() -> SignalListItemFactory {
    let factory = SignalListItemFactory::new();
    factory.connect_setup(|_, list_item| {
        let item = list_item.downcast_ref::<ListItem>().unwrap();
        let label = Label::new(None);
        label.set_halign(gtk4::Align::Start);
        item.set_child(Some(&label));
    });
    factory.connect_bind(|_, list_item| {
        let item = list_item.downcast_ref::<ListItem>().unwrap();
        let obj = item.item().and_downcast::<StringObject>().unwrap();
        let raw = obj.string();
        let label = item.child().and_downcast::<Label>().unwrap();
        let code = decode_vcs_code(&raw);
        label.set_label(vcs_status_label(code));
        for cls in &["diff-changed", "diff-deleted", "diff-inserted"] {
            label.remove_css_class(cls);
        }
        let css = vcs_status_css(code);
        if !css.is_empty() {
            label.add_css_class(css);
        }
    });
    factory
}

fn make_vcs_path_factory() -> SignalListItemFactory {
    let factory = SignalListItemFactory::new();
    factory.connect_setup(|_, list_item| {
        let item = list_item.downcast_ref::<ListItem>().unwrap();
        let hbox = GtkBox::new(Orientation::Horizontal, 4);
        let icon = Image::new();
        let label = Label::new(None);
        label.set_halign(gtk4::Align::Start);
        label.set_hexpand(true);
        hbox.append(&icon);
        hbox.append(&label);
        item.set_child(Some(&hbox));
    });
    factory.connect_bind(|_, list_item| {
        let item = list_item.downcast_ref::<ListItem>().unwrap();
        let obj = item.item().and_downcast::<StringObject>().unwrap();
        let raw = obj.string();
        let hbox = item.child().and_downcast::<GtkBox>().unwrap();
        let icon = hbox.first_child().and_downcast::<Image>().unwrap();
        let label = icon.next_sibling().and_downcast::<Label>().unwrap();
        let code = decode_vcs_code(&raw);
        let path = decode_vcs_path(&raw);
        icon.set_icon_name(Some("text-x-generic-symbolic"));
        label.set_label(path);
        for cls in &["diff-changed", "diff-deleted", "diff-inserted"] {
            label.remove_css_class(cls);
        }
        let css = vcs_status_css(code);
        if !css.is_empty() {
            label.add_css_class(css);
        }
    });
    factory
}

fn open_vcs_diff(
    notebook: &Notebook,
    rel_path: &str,
    status_code: &str,
    open_tabs: &Rc<RefCell<Vec<FileTab>>>,
    repo_root: &Path,
    temp_dir: &Path,
    settings: &Rc<RefCell<Settings>>,
) {
    // If already open, switch to it
    if let Some(idx) = open_tabs
        .borrow()
        .iter()
        .position(|t| t.rel_path == rel_path)
    {
        // +1 because page 0 is the VCS list tab
        notebook.set_current_page(Some((idx + 1) as u32));
        return;
    }

    let working_path = repo_root.join(rel_path);

    let (left_path, right_path) = match status_code {
        "M" | "R" => {
            let head = crate::vcs::head_content(repo_root, rel_path).unwrap_or_default();
            let temp_file = temp_dir.join(rel_path);
            if let Some(parent) = temp_file.parent() {
                let _ = fs::create_dir_all(parent);
            }
            let _ = fs::write(&temp_file, &head);
            (temp_file, working_path)
        }
        "A" | "U" => {
            let temp_file = temp_dir.join(format!("__empty__{rel_path}"));
            if let Some(parent) = temp_file.parent() {
                let _ = fs::create_dir_all(parent);
            }
            let _ = fs::write(&temp_file, "");
            (temp_file, working_path)
        }
        "D" => {
            let head = crate::vcs::head_content(repo_root, rel_path).unwrap_or_default();
            let temp_left = temp_dir.join(rel_path);
            let temp_right = temp_dir.join(format!("__deleted__{rel_path}"));
            for p in [&temp_left, &temp_right] {
                if let Some(parent) = p.parent() {
                    let _ = fs::create_dir_all(parent);
                }
            }
            let _ = fs::write(&temp_left, &head);
            let _ = fs::write(&temp_right, "");
            (temp_left, temp_right)
        }
        _ => return,
    };

    let labels = vec![format!("HEAD: {rel_path}"), format!("Working: {rel_path}")];
    let dv = build_diff_view(&left_path, &right_path, &labels, settings);
    dv.widget
        .insert_action_group("diff", Some(&dv.action_group));

    let tab_id = NEXT_TAB_ID.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
    open_tabs.borrow_mut().push(FileTab {
        id: tab_id,
        rel_path: rel_path.to_string(),
        left_buf: dv.left_buf,
        right_buf: dv.right_buf,
        left_save: dv.left_save,
        right_save: dv.right_save,
    });

    let status_text = vcs_status_label(status_code);
    let file_name = Path::new(rel_path).file_name().map_or_else(
        || rel_path.to_string(),
        |n| n.to_string_lossy().into_owned(),
    );
    let tab_title = format!("[{status_text}] {file_name}");

    let tab_label_box = GtkBox::new(Orientation::Horizontal, 4);
    let label = Label::new(Some(&tab_title));
    let close_btn = Button::from_icon_name("window-close-symbolic");
    close_btn.set_has_frame(false);
    tab_label_box.append(&label);
    tab_label_box.append(&close_btn);

    let page_num = notebook.append_page(&dv.widget, Some(&tab_label_box));
    notebook.set_current_page(Some(page_num));

    let nb = notebook.clone();
    let w = dv.widget.clone();
    let tabs = open_tabs.clone();
    close_btn.connect_clicked(move |_| {
        if let Some(n) = nb.page_num(&w) {
            nb.remove_page(Some(n));
        }
        tabs.borrow_mut().retain(|t| t.id != tab_id);
    });
}

fn build_vcs_window(app: &Application, dir: std::path::PathBuf, settings: Rc<RefCell<Settings>>) {
    let Some(repo_root) = crate::vcs::repo_root(&dir) else {
        eprintln!("Error: could not determine git repository root");
        return;
    };

    let temp_dir = std::env::temp_dir().join(format!("meld-rs-{}", std::process::id()));
    let _ = fs::create_dir_all(&temp_dir);

    // Scan changed files
    let entries = crate::vcs::changed_files(&repo_root);
    let store = ListStore::new::<StringObject>();
    let mut initial_encoded = Vec::new();
    for entry in &entries {
        let encoded = encode_vcs_row(&entry.status, &entry.rel_path);
        store.append(&StringObject::new(&encoded));
        initial_encoded.push(encoded);
    }
    let last_encoded: Rc<RefCell<Vec<String>>> = Rc::new(RefCell::new(initial_encoded));

    let sel = SingleSelection::new(Some(store.clone()));
    let view = ColumnView::new(Some(sel.clone()));
    view.set_show_column_separators(true);
    view.set_show_row_separators(true);

    let status_col = ColumnViewColumn::new(Some("Status"), Some(make_vcs_status_factory()));
    status_col.set_fixed_width(100);
    view.append_column(&status_col);

    let path_col = ColumnViewColumn::new(Some("File"), Some(make_vcs_path_factory()));
    path_col.set_expand(true);
    view.append_column(&path_col);

    let list_scroll = ScrolledWindow::builder().vexpand(true).child(&view).build();

    // Toolbar
    let toolbar = GtkBox::new(Orientation::Horizontal, 8);
    toolbar.set_margin_start(6);
    toolbar.set_margin_end(6);
    toolbar.set_margin_top(4);
    toolbar.set_margin_bottom(4);

    let repo_label = Label::new(Some(&repo_root.to_string_lossy()));
    repo_label.set_halign(gtk4::Align::Start);
    repo_label.set_hexpand(true);
    repo_label.add_css_class("dim-label");

    let count_label = Label::new(Some(&format!(
        "{} changed file{}",
        entries.len(),
        if entries.len() == 1 { "" } else { "s" }
    )));
    count_label.add_css_class("chunk-label");

    let refresh_btn = Button::from_icon_name("view-refresh-symbolic");
    refresh_btn.set_tooltip_text(Some("Refresh"));

    let prefs_btn = Button::from_icon_name("preferences-system-symbolic");
    prefs_btn.set_tooltip_text(Some("Preferences (Ctrl+,)"));
    prefs_btn.set_action_name(Some("win.prefs"));

    toolbar.append(&repo_label);
    toolbar.append(&count_label);
    toolbar.append(&refresh_btn);
    toolbar.append(&prefs_btn);

    // VCS tab
    let vcs_tab = GtkBox::new(Orientation::Vertical, 0);
    vcs_tab.append(&toolbar);
    vcs_tab.append(&gtk4::Separator::new(Orientation::Horizontal));
    vcs_tab.append(&list_scroll);
    vcs_tab.set_vexpand(true);

    // Notebook
    let notebook = Notebook::new();
    notebook.set_scrollable(true);
    notebook.append_page(&vcs_tab, Some(&Label::new(Some("Changes"))));

    let open_tabs: Rc<RefCell<Vec<FileTab>>> = Rc::new(RefCell::new(Vec::new()));

    // Open selected item helper (shared by double-click and Enter key)
    let open_selected = {
        let nb = notebook.clone();
        let tabs = open_tabs.clone();
        let rr = repo_root.clone();
        let td = temp_dir.clone();
        let st = settings.clone();
        let store_ref = store.clone();
        let s = sel.clone();
        Rc::new(move |pos: Option<u32>| {
            // Try position-based lookup first, fall back to selected item
            let item = pos
                .and_then(|p| store_ref.item(p))
                .or_else(|| s.selected_item());
            if let Some(item) = item {
                let obj = item.downcast::<StringObject>().unwrap();
                let raw = obj.string();
                open_vcs_diff(
                    &nb,
                    decode_vcs_path(&raw),
                    decode_vcs_code(&raw),
                    &tabs,
                    &rr,
                    &td,
                    &st,
                );
            }
        })
    };

    // Double-click handler
    {
        let open = open_selected.clone();
        view.connect_activate(move |_v, pos| {
            open(Some(pos));
        });
    }

    // Enter key handler
    {
        let open = open_selected.clone();
        let key_ctl = EventControllerKey::new();
        key_ctl.connect_key_pressed(move |_, key, _, _| {
            if key == gtk4::gdk::Key::Return || key == gtk4::gdk::Key::KP_Enter {
                open(None);
                return gtk4::glib::Propagation::Stop;
            }
            gtk4::glib::Propagation::Proceed
        });
        view.add_controller(key_ctl);
    }

    // Refresh handler
    let refresh: Rc<dyn Fn()> = {
        let rr = repo_root.clone();
        let st_ref = store.clone();
        let cl = count_label.clone();
        let le = last_encoded.clone();
        Rc::new(move || {
            let entries = crate::vcs::changed_files(&rr);
            let new_encoded: Vec<String> = entries
                .iter()
                .map(|e| encode_vcs_row(&e.status, &e.rel_path))
                .collect();
            if new_encoded == *le.borrow() {
                return;
            }
            le.borrow_mut().clone_from(&new_encoded);
            st_ref.remove_all();
            for encoded in &new_encoded {
                st_ref.append(&StringObject::new(encoded));
            }
            cl.set_label(&format!(
                "{} changed file{}",
                entries.len(),
                if entries.len() == 1 { "" } else { "s" }
            ));
        })
    };
    {
        let r = refresh.clone();
        refresh_btn.connect_clicked(move |_| r());
    }

    // File watcher
    {
        let (fs_tx, fs_rx) = mpsc::channel::<()>();
        {
            let rr = repo_root.clone();
            std::thread::spawn(move || {
                use notify::{RecursiveMode, Watcher};
                let _watcher = {
                    let mut w = notify::recommended_watcher(
                        move |res: Result<notify::Event, notify::Error>| {
                            if let Ok(event) = res {
                                // Skip events inside .git directory (git status touches
                                // index files, which would cause an infinite refresh loop)
                                let dominated_by_git = event
                                    .paths
                                    .iter()
                                    .all(|p| p.components().any(|c| c.as_os_str() == ".git"));
                                if !dominated_by_git {
                                    let _ = fs_tx.send(());
                                }
                            }
                        },
                    )
                    .expect("Failed to create file watcher");
                    w.watch(&rr, RecursiveMode::Recursive).ok();
                    w
                };
                loop {
                    std::thread::park();
                }
            });
        }
        let r = refresh.clone();
        gtk4::glib::timeout_add_local(Duration::from_millis(500), move || {
            let mut changed = false;
            while fs_rx.try_recv().is_ok() {
                changed = true;
            }
            if changed {
                r();
            }
            gtk4::glib::ControlFlow::Continue
        });
    }

    // ── VCS context menu ──────────────────────────────────────────
    let vcs_action_group = gio::SimpleActionGroup::new();
    {
        let action = gio::SimpleAction::new("open-diff", None);
        let open = open_selected.clone();
        action.connect_activate(move |_, _| {
            open(None);
        });
        vcs_action_group.add_action(&action);
    }
    {
        let action = gio::SimpleAction::new("discard", None);
        let s = sel.clone();
        let rr = repo_root.clone();
        let r = refresh.clone();
        action.connect_activate(move |_, _| {
            if let Some(item) = s.selected_item() {
                let obj = item.downcast::<StringObject>().unwrap();
                let raw = obj.string();
                crate::vcs::discard_changes(&rr, decode_vcs_path(&raw));
                r();
            }
        });
        vcs_action_group.add_action(&action);
    }
    {
        let action = gio::SimpleAction::new("stage", None);
        let s = sel.clone();
        let rr = repo_root.clone();
        let r = refresh.clone();
        action.connect_activate(move |_, _| {
            if let Some(item) = s.selected_item() {
                let obj = item.downcast::<StringObject>().unwrap();
                let raw = obj.string();
                crate::vcs::stage_file(&rr, decode_vcs_path(&raw));
                r();
            }
        });
        vcs_action_group.add_action(&action);
    }
    {
        let action = gio::SimpleAction::new("trash", None);
        let s = sel.clone();
        let rr = repo_root.clone();
        let r = refresh.clone();
        action.connect_activate(move |_, _| {
            if let Some(item) = s.selected_item() {
                let obj = item.downcast::<StringObject>().unwrap();
                let raw = obj.string();
                let path = rr.join(decode_vcs_path(&raw));
                let _ = gio::File::for_path(&path).trash(gio::Cancellable::NONE);
                r();
            }
        });
        vcs_action_group.add_action(&action);
    }
    vcs_tab.insert_action_group("vcs", Some(&vcs_action_group));

    let vcs_menu = gio::Menu::new();
    vcs_menu.append(Some("Open Diff"), Some("vcs.open-diff"));
    vcs_menu.append(Some("Discard Changes"), Some("vcs.discard"));
    vcs_menu.append(Some("Stage"), Some("vcs.stage"));
    vcs_menu.append(Some("Trash"), Some("vcs.trash"));

    let vcs_popover = PopoverMenu::from_model(Some(&vcs_menu));
    vcs_popover.set_parent(&view);
    vcs_popover.set_has_arrow(false);

    {
        let gesture = GestureClick::new();
        gesture.set_button(3);
        let s = sel.clone();
        let act_open = vcs_action_group
            .lookup_action("open-diff")
            .and_downcast::<gio::SimpleAction>()
            .unwrap();
        let act_discard = vcs_action_group
            .lookup_action("discard")
            .and_downcast::<gio::SimpleAction>()
            .unwrap();
        let act_trash = vcs_action_group
            .lookup_action("trash")
            .and_downcast::<gio::SimpleAction>()
            .unwrap();
        let pop = vcs_popover.clone();
        gesture.connect_pressed(move |_, _, x, y| {
            if let Some(item) = s.selected_item() {
                let obj = item.downcast::<StringObject>().unwrap();
                let raw = obj.string();
                let code = decode_vcs_code(&raw);
                let untracked = code == "U";
                act_open.set_enabled(!untracked);
                act_discard.set_enabled(!untracked);
                act_trash.set_enabled(untracked);
                pop.set_pointing_to(Some(&gtk4::gdk::Rectangle::new(x as i32, y as i32, 1, 1)));
                pop.popup();
            }
        });
        view.add_controller(gesture);
    }

    // Window
    let repo_name = repo_root.file_name().map_or_else(
        || repo_root.to_string_lossy().into_owned(),
        |n| n.to_string_lossy().into_owned(),
    );
    let window = ApplicationWindow::builder()
        .application(app)
        .title(format!("meld-rs — {repo_name} (git)"))
        .default_width(700)
        .default_height(500)
        .child(&notebook)
        .build();

    // Preferences action
    let win_actions = gio::SimpleActionGroup::new();
    {
        let action = gio::SimpleAction::new("prefs", None);
        let w = window.clone();
        let st = settings.clone();
        action.connect_activate(move |_, _| {
            show_preferences(&w, &st);
        });
        win_actions.add_action(&action);
    }
    window.insert_action_group("win", Some(&win_actions));

    if let Some(gtk_app) = window.application() {
        gtk_app.set_accels_for_action("win.prefs", &["<Ctrl>comma"]);
    }

    // Clean up temp dir on destroy
    let td = temp_dir.clone();
    window.connect_destroy(move |_| {
        let _ = fs::remove_dir_all(&td);
    });

    window.present();
    view.grab_focus();
}

// ─── Welcome window ─────────────────────────────────────────────────────────

fn build_welcome_window(app: &Application, settings: Rc<RefCell<Settings>>) {
    let window = ApplicationWindow::builder()
        .application(app)
        .title("meld-rs")
        .default_width(480)
        .default_height(360)
        .build();

    let content = GtkBox::new(Orientation::Vertical, 16);
    content.set_margin_top(32);
    content.set_margin_bottom(32);
    content.set_margin_start(48);
    content.set_margin_end(48);
    content.set_valign(gtk4::Align::Center);

    let title = Label::new(Some("meld-rs"));
    title.add_css_class("title-1");
    content.append(&title);

    let subtitle = Label::new(Some("Visual Diff and Merge Tool"));
    subtitle.add_css_class("dim-label");
    content.append(&subtitle);

    let spacer = GtkBox::new(Orientation::Vertical, 0);
    spacer.set_margin_top(8);
    content.append(&spacer);

    // Compare Files button
    let files_btn = make_welcome_button("Compare Files", "Compare two files side-by-side");
    content.append(&files_btn);

    // Compare Directories button
    let dirs_btn = make_welcome_button("Compare Directories", "Compare directory trees");
    content.append(&dirs_btn);

    // 3-way Merge button
    let merge_btn = make_welcome_button("3-way Merge", "Merge three files");
    content.append(&merge_btn);

    // Compare Files handler
    {
        let app2 = app.clone();
        let w = window.clone();
        let st = settings.clone();
        files_btn.connect_clicked(move |_| {
            let dialog = gtk4::FileDialog::new();
            dialog.set_title("Select first file");
            let app3 = app2.clone();
            let w2 = w.clone();
            let st2 = st.clone();
            dialog.open(Some(&w), gio::Cancellable::NONE, move |result| {
                if let Ok(first) = result
                    && let Some(first_path) = first.path()
                {
                    let dialog2 = gtk4::FileDialog::new();
                    dialog2.set_title("Select second file");
                    let app4 = app3.clone();
                    let w3 = w2.clone();
                    let st3 = st2.clone();
                    dialog2.open(Some(&w2), gio::Cancellable::NONE, move |result2| {
                        if let Ok(second) = result2
                            && let Some(second_path) = second.path()
                        {
                            build_file_window(&app4, first_path, second_path, &[], &st3);
                            w3.close();
                        }
                    });
                }
            });
        });
    }

    // Compare Directories handler
    {
        let app2 = app.clone();
        let w = window.clone();
        let st = settings.clone();
        dirs_btn.connect_clicked(move |_| {
            let dialog = gtk4::FileDialog::new();
            dialog.set_title("Select first directory");
            let app3 = app2.clone();
            let w2 = w.clone();
            let st2 = st.clone();
            dialog.select_folder(Some(&w), gio::Cancellable::NONE, move |result| {
                if let Ok(first) = result
                    && let Some(first_path) = first.path()
                {
                    let dialog2 = gtk4::FileDialog::new();
                    dialog2.set_title("Select second directory");
                    let app4 = app3.clone();
                    let w3 = w2.clone();
                    let st3 = st2.clone();
                    dialog2.select_folder(Some(&w2), gio::Cancellable::NONE, move |result2| {
                        if let Ok(second) = result2
                            && let Some(second_path) = second.path()
                        {
                            build_dir_window(&app4, first_path, second_path, &[], st3);
                            w3.close();
                        }
                    });
                }
            });
        });
    }

    // 3-way Merge handler
    {
        let app2 = app.clone();
        let w = window.clone();
        let st = settings.clone();
        merge_btn.connect_clicked(move |_| {
            let dialog = gtk4::FileDialog::new();
            dialog.set_title("Select left file");
            let app3 = app2.clone();
            let w2 = w.clone();
            let st2 = st.clone();
            dialog.open(Some(&w), gio::Cancellable::NONE, move |result| {
                if let Ok(first) = result
                    && let Some(first_path) = first.path()
                {
                    let dialog2 = gtk4::FileDialog::new();
                    dialog2.set_title("Select middle (base) file");
                    let app4 = app3.clone();
                    let w3 = w2.clone();
                    let st3 = st2.clone();
                    dialog2.open(Some(&w2), gio::Cancellable::NONE, move |result2| {
                        if let Ok(second) = result2
                            && let Some(second_path) = second.path()
                        {
                            let dialog3 = gtk4::FileDialog::new();
                            dialog3.set_title("Select right file");
                            let app5 = app4.clone();
                            let w4 = w3.clone();
                            let st4 = st3.clone();
                            dialog3.open(Some(&w3), gio::Cancellable::NONE, move |result3| {
                                if let Ok(third) = result3
                                    && let Some(third_path) = third.path()
                                {
                                    build_merge_window(
                                        &app5,
                                        first_path,
                                        second_path,
                                        third_path,
                                        None,
                                        &[],
                                        &st4,
                                    );
                                    w4.close();
                                }
                            });
                        }
                    });
                }
            });
        });
    }

    // Preferences
    let prefs_btn = Button::from_icon_name("preferences-system-symbolic");
    prefs_btn.set_tooltip_text(Some("Preferences (Ctrl+,)"));
    prefs_btn.set_action_name(Some("win.prefs"));
    prefs_btn.set_halign(gtk4::Align::End);
    prefs_btn.set_margin_top(8);
    content.append(&prefs_btn);

    let win_actions = gio::SimpleActionGroup::new();
    {
        let action = gio::SimpleAction::new("prefs", None);
        let w = window.clone();
        let st = settings.clone();
        action.connect_activate(move |_, _| {
            show_preferences(&w, &st);
        });
        win_actions.add_action(&action);
    }
    window.insert_action_group("win", Some(&win_actions));

    if let Some(gtk_app) = window.application() {
        gtk_app.set_accels_for_action("win.prefs", &["<Ctrl>comma"]);
    }

    window.set_child(Some(&content));
    window.present();
}

fn make_welcome_button(title_text: &str, subtitle_text: &str) -> Button {
    let bx = GtkBox::new(Orientation::Vertical, 2);
    bx.set_margin_top(8);
    bx.set_margin_bottom(8);
    bx.set_margin_start(8);
    bx.set_margin_end(8);
    let t = Label::new(Some(title_text));
    t.add_css_class("heading");
    let s = Label::new(Some(subtitle_text));
    s.add_css_class("dim-label");
    bx.append(&t);
    bx.append(&s);
    let btn = Button::new();
    btn.set_child(Some(&bx));
    btn
}

// ─── Main UI ───────────────────────────────────────────────────────────────

pub(crate) fn build_ui(application: &Application, mode: CompareMode) {
    application.connect_activate(move |app| {
        let mode = mode.clone();

        // Load CSS
        let provider = CssProvider::new();
        provider.load_from_string(CSS);
        gtk4::style_context_add_provider_for_display(
            &Display::default().unwrap(),
            &provider,
            gtk4::STYLE_PROVIDER_PRIORITY_APPLICATION,
        );

        let settings = Rc::new(RefCell::new(Settings::load()));

        match mode {
            CompareMode::Dirs {
                left,
                right,
                labels,
            } => build_dir_window(app, left, right, &labels, settings),
            CompareMode::Files {
                left,
                right,
                labels,
            } => build_file_window(app, left, right, &labels, &settings),
            CompareMode::Merge {
                left,
                middle,
                right,
                output,
                labels,
            } => build_merge_window(app, left, middle, right, output, &labels, &settings),
            CompareMode::Vcs { dir } => build_vcs_window(app, dir, settings),
            CompareMode::Welcome => build_welcome_window(app, settings),
        }
    });
}

// ─── Chunk navigation helpers ──────────────────────────────────────────────

fn count_changes(chunks: &[DiffChunk]) -> usize {
    chunks.iter().filter(|c| c.tag != DiffTag::Equal).count()
}

fn navigate_chunk(
    chunks: &[DiffChunk],
    current_chunk: &Rc<Cell<Option<usize>>>,
    direction: i32, // -1 = prev, +1 = next
    left_tv: &TextView,
    left_buf: &TextBuffer,
    left_scroll: &ScrolledWindow,
) {
    let non_equal: Vec<usize> = chunks
        .iter()
        .enumerate()
        .filter(|(_, c)| c.tag != DiffTag::Equal)
        .map(|(i, _)| i)
        .collect();

    if non_equal.is_empty() {
        return;
    }

    let next_idx = match current_chunk.get() {
        Some(cur) => {
            let pos = non_equal.iter().position(|&i| i == cur);
            match pos {
                Some(p) => {
                    if direction > 0 {
                        non_equal.get(p + 1).or(non_equal.first())
                    } else if p > 0 {
                        non_equal.get(p - 1)
                    } else {
                        non_equal.last()
                    }
                }
                None => {
                    if direction > 0 {
                        non_equal.first()
                    } else {
                        non_equal.last()
                    }
                }
            }
        }
        None => {
            if direction > 0 {
                non_equal.first()
            } else {
                non_equal.last()
            }
        }
    };

    if let Some(&idx) = next_idx {
        current_chunk.set(Some(idx));
        let chunk = &chunks[idx];
        // Scroll left pane to the chunk
        scroll_to_line(left_tv, left_buf, chunk.start_a, left_scroll);
    }
}

fn scroll_to_line(tv: &TextView, buf: &TextBuffer, line: usize, scroll: &ScrolledWindow) {
    if let Some(iter) = buf.iter_at_line(line as i32) {
        let (y, h) = tv.line_yrange(&iter);
        let visible_h = scroll.vadjustment().page_size();
        // Center the line in the visible area
        let target = (y as f64 + h as f64 / 2.0) - visible_h / 2.0;
        scroll.vadjustment().set_value(target.max(0.0));
    }
}

fn update_chunk_label(label: &Label, chunks: &[DiffChunk], current: Option<usize>) {
    let total = count_changes(chunks);
    if total == 0 {
        label.set_label("No changes");
        return;
    }
    let non_equal: Vec<usize> = chunks
        .iter()
        .enumerate()
        .filter(|(_, c)| c.tag != DiffTag::Equal)
        .map(|(i, _)| i)
        .collect();
    match current {
        Some(cur) => {
            if let Some(pos) = non_equal.iter().position(|&i| i == cur) {
                label.set_label(&format!("Change {} of {}", pos + 1, total));
            } else {
                label.set_label(&format!("{total} changes"));
            }
        }
        None => label.set_label(&format!("{total} changes")),
    }
}

// ─── Scroll synchronization ───────────────────────────────────────────────

fn setup_scroll_sync(
    left_scroll: &ScrolledWindow,
    right_scroll: &ScrolledWindow,
    gutter: &DrawingArea,
) {
    let syncing = Rc::new(Cell::new(false));

    // Left → Right
    {
        let rs = right_scroll.clone();
        let s = syncing.clone();
        let g = gutter.clone();
        left_scroll.vadjustment().connect_value_changed(move |adj| {
            if !s.get() {
                s.set(true);
                sync_adjustment(&rs.vadjustment(), adj);
                g.queue_draw();
                s.set(false);
            }
        });
    }

    // Right → Left
    {
        let ls = left_scroll.clone();
        let s = syncing.clone();
        let g = gutter.clone();
        right_scroll
            .vadjustment()
            .connect_value_changed(move |adj| {
                if !s.get() {
                    s.set(true);
                    sync_adjustment(&ls.vadjustment(), adj);
                    g.queue_draw();
                    s.set(false);
                }
            });
    }
}

fn sync_adjustment(target: &Adjustment, source: &Adjustment) {
    let src_upper = source.upper() - source.page_size();
    if src_upper <= 0.0 {
        return;
    }
    let ratio = source.value() / src_upper;
    let tgt_upper = target.upper() - target.page_size();
    target.set_value(ratio * tgt_upper);
}

// ─── File comparison window ────────────────────────────────────────────────

fn build_file_window(
    app: &Application,
    left_path: std::path::PathBuf,
    right_path: std::path::PathBuf,
    labels: &[String],
    settings: &Rc<RefCell<Settings>>,
) {
    let dv = build_diff_view(&left_path, &right_path, labels, settings);

    // File watcher for both files
    let (fs_tx, fs_rx) = mpsc::channel::<()>();
    {
        let lp = left_path.clone();
        let rp = right_path.clone();
        std::thread::spawn(move || {
            use notify::{RecursiveMode, Watcher};
            let _watcher = {
                let mut w = notify::recommended_watcher(
                    move |res: Result<notify::Event, notify::Error>| {
                        if res.is_ok() {
                            let _ = fs_tx.send(());
                        }
                    },
                )
                .expect("Failed to create file watcher");
                if let Some(parent) = lp.parent() {
                    w.watch(parent, RecursiveMode::NonRecursive).ok();
                }
                if let Some(parent) = rp.parent() {
                    w.watch(parent, RecursiveMode::NonRecursive).ok();
                }
                w
            };
            loop {
                std::thread::park();
            }
        });
    }

    // Poll for filesystem changes and reload
    {
        let lb = dv.left_buf.clone();
        let rb = dv.right_buf.clone();
        let lp = left_path.clone();
        let rp = right_path.clone();
        let l_save = dv.left_save.clone();
        let r_save = dv.right_save.clone();
        gtk4::glib::timeout_add_local(Duration::from_millis(500), move || {
            let mut changed = false;
            while fs_rx.try_recv().is_ok() {
                changed = true;
            }
            if changed
                && !SAVING.load(std::sync::atomic::Ordering::Relaxed)
                && !l_save.is_sensitive()
                && !r_save.is_sensitive()
            {
                let left_content = fs::read_to_string(&lp).unwrap_or_default();
                let right_content = fs::read_to_string(&rp).unwrap_or_default();

                let cur_left = lb.text(&lb.start_iter(), &lb.end_iter(), false);
                let cur_right = rb.text(&rb.start_iter(), &rb.end_iter(), false);

                if cur_left.as_str() != left_content || cur_right.as_str() != right_content {
                    // set_text triggers connect_changed which schedules
                    // refresh_diff with the current filter state.
                    lb.set_text(&left_content);
                    rb.set_text(&right_content);
                    l_save.set_sensitive(false);
                    r_save.set_sensitive(false);
                }
            }
            gtk4::glib::ControlFlow::Continue
        });
    }

    // Window title
    let left_name = left_path.file_name().map_or_else(
        || left_path.display().to_string(),
        |n| n.to_string_lossy().into_owned(),
    );
    let right_name = right_path.file_name().map_or_else(
        || right_path.display().to_string(),
        |n| n.to_string_lossy().into_owned(),
    );
    let title = format!("{left_name} — {right_name}");

    let window = ApplicationWindow::builder()
        .application(app)
        .title(&title)
        .default_width(900)
        .default_height(600)
        .child(&dv.widget)
        .build();
    window.insert_action_group("diff", Some(&dv.action_group));

    // Preferences action
    let win_actions = gio::SimpleActionGroup::new();
    {
        let action = gio::SimpleAction::new("prefs", None);
        let w = window.clone();
        let st = settings.clone();
        action.connect_activate(move |_, _| {
            show_preferences(&w, &st);
        });
        win_actions.add_action(&action);
    }
    window.insert_action_group("win", Some(&win_actions));

    // Bind keyboard accelerators
    if let Some(gtk_app) = window.application() {
        gtk_app.set_accels_for_action("diff.prev-chunk", &["<Alt>Up", "<Ctrl>e"]);
        gtk_app.set_accels_for_action("diff.next-chunk", &["<Alt>Down", "<Ctrl>d"]);
        gtk_app.set_accels_for_action("diff.find", &["<Ctrl>f"]);
        gtk_app.set_accels_for_action("diff.find-replace", &["<Ctrl>h"]);
        gtk_app.set_accels_for_action("diff.find-next", &["F3"]);
        gtk_app.set_accels_for_action("diff.find-prev", &["<Shift>F3"]);
        gtk_app.set_accels_for_action("diff.go-to-line", &["<Ctrl>l"]);
        gtk_app.set_accels_for_action("diff.export-patch", &["<Ctrl><Shift>p"]);
        gtk_app.set_accels_for_action("win.prefs", &["<Ctrl>comma"]);
    }

    window.present();
}

// ─── Directory comparison window ───────────────────────────────────────────

fn build_dir_window(
    app: &Application,
    left_dir: std::path::PathBuf,
    right_dir: std::path::PathBuf,
    _labels: &[String],
    settings: Rc<RefCell<Settings>>,
) {
    let left_dir = Rc::new(RefCell::new(left_dir.to_string_lossy().into_owned()));
    let right_dir = Rc::new(RefCell::new(right_dir.to_string_lossy().into_owned()));

    // Build tree data (Rc<RefCell<…>> so the watcher callback can rebuild)
    let children_map = Rc::new(RefCell::new(HashMap::new()));
    let root_store = ListStore::new::<StringObject>();
    {
        let (store, _) = scan_level(
            Path::new(left_dir.borrow().as_str()),
            Path::new(right_dir.borrow().as_str()),
            "",
            &mut children_map.borrow_mut(),
            &settings.borrow().dir_filters,
        );
        for i in 0..store.n_items() {
            if let Some(obj) = store.item(i) {
                root_store.append(&obj.downcast::<StringObject>().unwrap());
            }
        }
    }

    // Shared TreeListModel — both panes see the same tree structure
    let cm = children_map.clone();
    let tree_model = TreeListModel::new(root_store.clone(), false, false, move |item| {
        let obj = item.downcast_ref::<StringObject>()?;
        let raw = obj.string();
        if decode_is_dir(&raw) {
            let rel = decode_rel_path(&raw);
            cm.borrow()
                .get(rel)
                .cloned()
                .map(gio::prelude::Cast::upcast::<gio::ListModel>)
        } else {
            None
        }
    });

    // Shared selection model — only the focused pane uses it;
    // the other pane uses NoSelection so no highlight is shown.
    let dir_sel = SingleSelection::new(Some(tree_model.clone()));
    let no_sel = gtk4::NoSelection::new(Some(tree_model.clone()));

    // ── Left pane ──────────────────────────────────────────────────
    let left_view = ColumnView::new(Some(dir_sel.clone()));
    left_view.set_show_column_separators(true);
    {
        let col = ColumnViewColumn::new(Some("Name"), Some(make_name_factory(true)));
        col.set_expand(true);
        left_view.append_column(&col);
        let col = ColumnViewColumn::new(Some("Size"), Some(make_field_factory(true, 4)));
        col.set_fixed_width(80);
        left_view.append_column(&col);
        let col =
            ColumnViewColumn::new(Some("Modification time"), Some(make_field_factory(true, 5)));
        col.set_fixed_width(180);
        left_view.append_column(&col);
    }

    // ── Right pane ─────────────────────────────────────────────────
    let right_view = ColumnView::new(Some(no_sel.clone()));
    right_view.set_show_column_separators(true);
    {
        let col = ColumnViewColumn::new(Some("Name"), Some(make_name_factory(false)));
        col.set_expand(true);
        right_view.append_column(&col);
        let col = ColumnViewColumn::new(Some("Size"), Some(make_field_factory(false, 6)));
        col.set_fixed_width(80);
        right_view.append_column(&col);
        let col = ColumnViewColumn::new(
            Some("Modification time"),
            Some(make_field_factory(false, 7)),
        );
        col.set_fixed_width(180);
        right_view.append_column(&col);
    }

    // Track which pane was last focused (true = left, false = right).
    // Swap models so only the focused pane shows selection.
    let focused_left = Rc::new(Cell::new(true));
    let sel_syncing = Rc::new(Cell::new(false));
    left_view.add_css_class("dir-pane-focused");
    {
        let fl = focused_left.clone();
        let lv = left_view.clone();
        let rv = right_view.clone();
        let sel = dir_sel.clone();
        let ns = no_sel.clone();
        let fc = EventControllerFocus::new();
        fc.connect_enter(move |_| {
            if fl.get() {
                return;
            }
            fl.set(true);
            lv.add_css_class("dir-pane-focused");
            rv.remove_css_class("dir-pane-focused");
            lv.set_model(Some(&sel));
            rv.set_model(Some(&ns));
            let pos = sel.selected();
            let v = lv.clone();
            gtk4::glib::idle_add_local_once(move || {
                v.scroll_to(pos, None, gtk4::ListScrollFlags::FOCUS, None);
            });
        });
        left_view.add_controller(fc);
        let fl = focused_left.clone();
        let lv = left_view.clone();
        let rv = right_view.clone();
        let sel = dir_sel.clone();
        let ns = no_sel.clone();
        let fc = EventControllerFocus::new();
        fc.connect_enter(move |_| {
            if !fl.get() {
                return;
            }
            fl.set(false);
            rv.add_css_class("dir-pane-focused");
            lv.remove_css_class("dir-pane-focused");
            rv.set_model(Some(&sel));
            lv.set_model(Some(&ns));
            let pos = sel.selected();
            let v = rv.clone();
            gtk4::glib::idle_add_local_once(move || {
                v.scroll_to(pos, None, gtk4::ListScrollFlags::FOCUS, None);
            });
        });
        right_view.add_controller(fc);
    }

    // Left/Right arrow keys switch between panes
    {
        let rv = right_view.clone();
        let kc = EventControllerKey::new();
        kc.set_propagation_phase(gtk4::PropagationPhase::Capture);
        kc.connect_key_pressed(move |_, key, _, _| {
            if key == gtk4::gdk::Key::Right {
                rv.grab_focus();
                gtk4::glib::Propagation::Stop
            } else {
                gtk4::glib::Propagation::Proceed
            }
        });
        left_view.add_controller(kc);
        let lv = left_view.clone();
        let kc = EventControllerKey::new();
        kc.set_propagation_phase(gtk4::PropagationPhase::Capture);
        kc.connect_key_pressed(move |_, key, _, _| {
            if key == gtk4::gdk::Key::Left {
                lv.grab_focus();
                gtk4::glib::Propagation::Stop
            } else {
                gtk4::glib::Propagation::Proceed
            }
        });
        right_view.add_controller(kc);
    }

    // ScrolledWindows + Paned
    let left_scroll = ScrolledWindow::builder()
        .hscrollbar_policy(PolicyType::Automatic)
        .min_content_width(360)
        .child(&left_view)
        .build();
    let right_scroll = ScrolledWindow::builder()
        .hscrollbar_policy(PolicyType::Automatic)
        .min_content_width(360)
        .child(&right_view)
        .build();

    // Synchronize vertical scrolling between left and right directory panes
    {
        let syncing = Rc::new(Cell::new(false));
        let rs = right_scroll.clone();
        let s = syncing.clone();
        left_scroll.vadjustment().connect_value_changed(move |adj| {
            if !s.get() {
                s.set(true);
                rs.vadjustment().set_value(adj.value());
                s.set(false);
            }
        });
        let ls = left_scroll.clone();
        let s = syncing;
        right_scroll
            .vadjustment()
            .connect_value_changed(move |adj| {
                if !s.get() {
                    s.set(true);
                    ls.vadjustment().set_value(adj.value());
                    s.set(false);
                }
            });
    }

    let dir_paned = Paned::new(Orientation::Horizontal);
    dir_paned.set_start_child(Some(&left_scroll));
    dir_paned.set_end_child(Some(&right_scroll));

    // ── Directory toolbar with copy buttons ───────────────────────
    let copy_left_btn = Button::from_icon_name("go-previous-symbolic");
    copy_left_btn.set_tooltip_text(Some("Copy to left (Alt+Left)"));
    let copy_right_btn = Button::from_icon_name("go-next-symbolic");
    copy_right_btn.set_tooltip_text(Some("Copy to right (Alt+Right)"));
    let delete_btn = Button::from_icon_name("user-trash-symbolic");
    delete_btn.set_tooltip_text(Some("Delete selected (Delete)"));

    let dir_copy_box = GtkBox::new(Orientation::Horizontal, 0);
    dir_copy_box.add_css_class("linked");
    dir_copy_box.append(&copy_left_btn);
    dir_copy_box.append(&copy_right_btn);

    let dir_toolbar = GtkBox::new(Orientation::Horizontal, 8);
    dir_toolbar.set_margin_start(6);
    dir_toolbar.set_margin_end(6);
    dir_toolbar.set_margin_top(4);
    dir_toolbar.set_margin_bottom(4);
    let dir_swap_btn = Button::from_icon_name("object-flip-horizontal-symbolic");
    dir_swap_btn.set_tooltip_text(Some("Swap panes"));
    let dir_prefs_btn = Button::from_icon_name("preferences-system-symbolic");
    dir_prefs_btn.set_tooltip_text(Some("Preferences (Ctrl+,)"));
    dir_prefs_btn.set_action_name(Some("win.prefs"));

    dir_toolbar.append(&dir_copy_box);
    dir_toolbar.append(&delete_btn);
    dir_toolbar.append(&dir_swap_btn);
    dir_toolbar.append(&dir_prefs_btn);

    // Helper: rescan directories and refresh the tree model
    let reload_dir = {
        let cm = children_map.clone();
        let rs = root_store.clone();
        let ld = left_dir.clone();
        let rd = right_dir.clone();
        let st = settings.clone();
        let tm = tree_model.clone();
        let sel = dir_sel.clone();
        let syncing = sel_syncing.clone();
        let lv = left_view.clone();
        let rv = right_view.clone();
        move || {
            // Suppress selection sync during rebuild
            syncing.set(true);
            let saved_pos = sel.selected();
            let saved_rel = (|| -> Option<String> {
                let row = tm.item(saved_pos)?.downcast::<TreeListRow>().ok()?;
                let obj = row.item().and_downcast::<StringObject>()?;
                Some(decode_rel_path(&obj.string()).to_string())
            })();

            // Save expanded rel_paths
            let mut expanded: Vec<String> = Vec::new();
            for i in 0..tm.n_items() {
                if let Some(row) = tm.item(i).and_then(|o| o.downcast::<TreeListRow>().ok())
                    && row.is_expanded()
                    && let Some(obj) = row.item().and_downcast::<StringObject>()
                {
                    expanded.push(decode_rel_path(&obj.string()).to_string());
                }
            }

            // Rebuild
            let mut new_map = HashMap::new();
            let (new_store, _) = scan_level(
                Path::new(ld.borrow().as_str()),
                Path::new(rd.borrow().as_str()),
                "",
                &mut new_map,
                &st.borrow().dir_filters,
            );
            // Skip rebuild if root-level data hasn't changed
            let root_changed = new_store.n_items() != rs.n_items()
                || (0..new_store.n_items()).any(|i| {
                    new_store
                        .item(i)
                        .and_downcast::<StringObject>()
                        .map(|o| o.string())
                        != rs
                            .item(i)
                            .and_downcast::<StringObject>()
                            .map(|o| o.string())
                });
            // Always update children_map so re-expanded dirs show fresh data
            *cm.borrow_mut() = new_map;
            if !root_changed {
                syncing.set(false);
                return;
            }
            rs.remove_all();
            for i in 0..new_store.n_items() {
                if let Some(obj) = new_store.item(i) {
                    rs.append(&obj.downcast::<StringObject>().unwrap());
                }
            }

            // Restore expanded state (must iterate after each expand since
            // expanding a row inserts children and shifts positions)
            for rel in &expanded {
                for i in 0..tm.n_items() {
                    if let Some(row) = tm.item(i).and_then(|o| o.downcast::<TreeListRow>().ok())
                        && let Some(obj) = row.item().and_downcast::<StringObject>()
                        && decode_rel_path(&obj.string()) == rel.as_str()
                    {
                        row.set_expanded(true);
                        break;
                    }
                }
            }

            // Restore selection on both panes
            let n = tm.n_items();
            let mut final_pos = saved_pos.min(if n > 0 { n - 1 } else { 0 });
            if let Some(ref rel) = saved_rel {
                for i in 0..n {
                    if let Some(row) = tm.item(i).and_then(|o| o.downcast::<TreeListRow>().ok())
                        && let Some(obj) = row.item().and_downcast::<StringObject>()
                        && decode_rel_path(&obj.string()) == rel.as_str()
                    {
                        final_pos = i;
                        break;
                    }
                }
            }
            if n > 0 {
                sel.set_selected(final_pos);
            }
            syncing.set(false);

            // Defer scroll_to until after GTK has laid out the new rows,
            // otherwise the ColumnView hasn't realized the items yet.
            let lv2 = lv.clone();
            let rv2 = rv.clone();
            gtk4::glib::idle_add_local_once(move || {
                let flags = gtk4::ListScrollFlags::FOCUS;
                lv2.scroll_to(final_pos, None, flags, None);
                rv2.scroll_to(final_pos, None, flags, None);
            });
        }
    };

    // Swap panes: swap left/right directories and rescan
    {
        let ld = left_dir.clone();
        let rd = right_dir.clone();
        let reload = reload_dir.clone();
        dir_swap_btn.connect_clicked(move |_| {
            let tmp = ld.borrow().clone();
            (*ld.borrow_mut()).clone_from(&rd.borrow());
            *rd.borrow_mut() = tmp;
            reload();
        });
    }

    // Helper: get selected row's encoded data
    let get_selected_row = {
        let tm = tree_model.clone();
        let sel = dir_sel.clone();
        move || -> Option<String> {
            let pos = sel.selected();
            let item = tm.item(pos)?;
            let row = item.downcast::<TreeListRow>().ok()?;
            let obj = row.item().and_downcast::<StringObject>()?;
            Some(obj.string().to_string())
        }
    };

    // Copy to left: right → left
    {
        let get_row = get_selected_row.clone();
        let ld = left_dir.clone();
        let rd = right_dir.clone();
        let reload = reload_dir.clone();
        copy_left_btn.connect_clicked(move |_| {
            if let Some(raw) = get_row() {
                let rel = decode_rel_path(&raw);
                let status = decode_status(&raw);
                if status == "R" || status == "D" {
                    let src = Path::new(rd.borrow().as_str()).join(rel);
                    let dst = Path::new(ld.borrow().as_str()).join(rel);
                    let _ = copy_path_recursive(&src, &dst);
                    reload();
                }
            }
        });
    }

    // Copy to right: left → right
    {
        let get_row = get_selected_row.clone();
        let ld = left_dir.clone();
        let rd = right_dir.clone();
        let reload = reload_dir.clone();
        copy_right_btn.connect_clicked(move |_| {
            if let Some(raw) = get_row() {
                let rel = decode_rel_path(&raw);
                let status = decode_status(&raw);
                if status == "L" || status == "D" {
                    let src = Path::new(ld.borrow().as_str()).join(rel);
                    let dst = Path::new(rd.borrow().as_str()).join(rel);
                    let _ = copy_path_recursive(&src, &dst);
                    reload();
                }
            }
        });
    }

    // Delete selected (trash; for items on both sides, use focused pane)
    {
        let get_row = get_selected_row.clone();
        let ld = left_dir.clone();
        let rd = right_dir.clone();
        let reload = reload_dir.clone();
        let fl = focused_left.clone();
        delete_btn.connect_clicked(move |_| {
            if let Some(raw) = get_row() {
                let rel = decode_rel_path(&raw);
                let status = decode_status(&raw);
                let lp = Path::new(ld.borrow().as_str()).join(rel);
                let rp = Path::new(rd.borrow().as_str()).join(rel);
                let path = match status {
                    "L" => Some(lp),
                    "R" => Some(rp),
                    "D" | "S" => Some(if fl.get() { lp } else { rp }),
                    _ => None,
                };
                if let Some(p) = path {
                    if let Err(e) = gio::File::for_path(&p).trash(gio::Cancellable::NONE) {
                        eprintln!("Trash failed: {e}");
                    }
                    reload();
                }
            }
        });
    }

    // Directory action group for keyboard shortcuts
    let dir_action_group = gio::SimpleActionGroup::new();
    {
        let action = gio::SimpleAction::new("folder-copy-left", None);
        let get_row = get_selected_row.clone();
        let ld = left_dir.clone();
        let rd = right_dir.clone();
        let reload = reload_dir.clone();
        action.connect_activate(move |_, _| {
            if let Some(raw) = get_row() {
                let rel = decode_rel_path(&raw);
                let status = decode_status(&raw);
                if status == "R" || status == "D" {
                    let src = Path::new(rd.borrow().as_str()).join(rel);
                    let dst = Path::new(ld.borrow().as_str()).join(rel);
                    let _ = copy_path_recursive(&src, &dst);
                    reload();
                }
            }
        });
        dir_action_group.add_action(&action);
    }
    {
        let action = gio::SimpleAction::new("folder-copy-right", None);
        let get_row = get_selected_row.clone();
        let ld = left_dir.clone();
        let rd = right_dir.clone();
        let reload = reload_dir.clone();
        action.connect_activate(move |_, _| {
            if let Some(raw) = get_row() {
                let rel = decode_rel_path(&raw);
                let status = decode_status(&raw);
                if status == "L" || status == "D" {
                    let src = Path::new(ld.borrow().as_str()).join(rel);
                    let dst = Path::new(rd.borrow().as_str()).join(rel);
                    let _ = copy_path_recursive(&src, &dst);
                    reload();
                }
            }
        });
        dir_action_group.add_action(&action);
    }
    {
        let action = gio::SimpleAction::new("folder-delete", None);
        let get_row = get_selected_row.clone();
        let ld = left_dir.clone();
        let rd = right_dir.clone();
        let reload = reload_dir.clone();
        let fl = focused_left.clone();
        action.connect_activate(move |_, _| {
            if let Some(raw) = get_row() {
                let rel = decode_rel_path(&raw);
                let status = decode_status(&raw);
                let lp = Path::new(ld.borrow().as_str()).join(rel);
                let rp = Path::new(rd.borrow().as_str()).join(rel);
                let path = match status {
                    "L" => Some(lp),
                    "R" => Some(rp),
                    "D" | "S" => Some(if fl.get() { lp } else { rp }),
                    _ => None,
                };
                if let Some(p) = path {
                    if let Err(e) = gio::File::for_path(&p).trash(gio::Cancellable::NONE) {
                        eprintln!("Trash failed: {e}");
                    }
                    reload();
                }
            }
        });
        dir_action_group.add_action(&action);
    }

    // ── Dir tab: toolbar + paned ──────────────────────────────────
    let dir_tab = GtkBox::new(Orientation::Vertical, 0);
    dir_tab.append(&dir_toolbar);
    dir_tab.append(&gtk4::Separator::new(Orientation::Horizontal));
    dir_tab.append(&dir_paned);
    dir_tab.set_vexpand(true);
    dir_paned.set_vexpand(true);
    dir_tab.insert_action_group("dir", Some(&dir_action_group));

    // ── Notebook (tabs) ────────────────────────────────────────────
    let notebook = Notebook::new();
    notebook.set_scrollable(true);
    notebook.append_page(&dir_tab, Some(&Label::new(Some("Directory"))));

    // Open file tabs tracking
    let open_tabs: Rc<RefCell<Vec<FileTab>>> = Rc::new(RefCell::new(Vec::new()));

    // Add "folder-open-diff" action now that notebook and open_tabs exist
    {
        let action = gio::SimpleAction::new("folder-open-diff", None);
        let get_row = get_selected_row.clone();
        let nb = notebook.clone();
        let tabs = open_tabs.clone();
        let ld = left_dir.clone();
        let rd = right_dir.clone();
        let st = settings.clone();
        action.connect_activate(move |_, _| {
            if let Some(raw) = get_row()
                && !decode_is_dir(&raw)
            {
                open_file_diff(
                    &nb,
                    decode_rel_path(&raw),
                    decode_status(&raw),
                    &tabs,
                    &ld.borrow(),
                    &rd.borrow(),
                    &st,
                );
            }
        });
        dir_action_group.add_action(&action);
    }

    // ── Dir context menu ────────────────────────────────────────────
    {
        let dir_menu = gio::Menu::new();
        dir_menu.append(Some("Open Diff"), Some("dir.folder-open-diff"));
        dir_menu.append(Some("Copy to Left"), Some("dir.folder-copy-left"));
        dir_menu.append(Some("Copy to Right"), Some("dir.folder-copy-right"));
        dir_menu.append(Some("Delete"), Some("dir.folder-delete"));

        let dir_popover_l = PopoverMenu::from_model(Some(&dir_menu));
        dir_popover_l.set_parent(&left_view);
        dir_popover_l.set_has_arrow(false);
        let dir_popover_r = PopoverMenu::from_model(Some(&dir_menu));
        dir_popover_r.set_parent(&right_view);
        dir_popover_r.set_has_arrow(false);

        let act_open = dir_action_group
            .lookup_action("folder-open-diff")
            .and_downcast::<gio::SimpleAction>()
            .unwrap();
        let act_left = dir_action_group
            .lookup_action("folder-copy-left")
            .and_downcast::<gio::SimpleAction>()
            .unwrap();
        let act_right = dir_action_group
            .lookup_action("folder-copy-right")
            .and_downcast::<gio::SimpleAction>()
            .unwrap();

        let setup_dir_ctx = |view: &ColumnView, popover: PopoverMenu| {
            let gesture = GestureClick::new();
            gesture.set_button(3);
            let get_row = get_selected_row.clone();
            let ao = act_open.clone();
            let al = act_left.clone();
            let ar = act_right.clone();
            let pop = popover;
            gesture.connect_pressed(move |_, _, x, y| {
                if let Some(raw) = get_row() {
                    let is_dir = decode_is_dir(&raw);
                    let status = decode_status(&raw);
                    ao.set_enabled(!is_dir && (status == "D" || status == "S"));
                    al.set_enabled(status == "R" || status == "D");
                    ar.set_enabled(status == "L" || status == "D");
                    pop.set_pointing_to(Some(&gtk4::gdk::Rectangle::new(x as i32, y as i32, 1, 1)));
                    pop.popup();
                }
            });
            view.add_controller(gesture);
        };
        setup_dir_ctx(&left_view, dir_popover_l);
        setup_dir_ctx(&right_view, dir_popover_r);
    }

    // Activate handlers — double-click a file row to open diff in new tab
    {
        let nb = notebook.clone();
        let tm = tree_model.clone();
        let tabs = open_tabs.clone();
        let ld = left_dir.clone();
        let rd = right_dir.clone();
        let st = settings.clone();
        left_view.connect_activate(move |_, pos| {
            if let Some(item) = tm.item(pos) {
                let row = item.downcast::<TreeListRow>().unwrap();
                let obj = row.item().and_downcast::<StringObject>().unwrap();
                let raw = obj.string();
                if !decode_is_dir(&raw) {
                    open_file_diff(
                        &nb,
                        decode_rel_path(&raw),
                        decode_status(&raw),
                        &tabs,
                        &ld.borrow(),
                        &rd.borrow(),
                        &st,
                    );
                }
            }
        });
    }
    {
        let nb = notebook.clone();
        let tm = tree_model.clone();
        let tabs = open_tabs.clone();
        let ld = left_dir.clone();
        let rd = right_dir.clone();
        let st = settings.clone();
        right_view.connect_activate(move |_, pos| {
            if let Some(item) = tm.item(pos) {
                let row = item.downcast::<TreeListRow>().unwrap();
                let obj = row.item().and_downcast::<StringObject>().unwrap();
                let raw = obj.string();
                if !decode_is_dir(&raw) {
                    open_file_diff(
                        &nb,
                        decode_rel_path(&raw),
                        decode_status(&raw),
                        &tabs,
                        &ld.borrow(),
                        &rd.borrow(),
                        &st,
                    );
                }
            }
        });
    }

    // ── File watcher ───────────────────────────────────────────────
    let (fs_tx, fs_rx) = mpsc::channel::<()>();
    {
        let ld = left_dir.borrow().clone();
        let rd = right_dir.borrow().clone();
        std::thread::spawn(move || {
            use notify::{RecursiveMode, Watcher};
            let _watcher = {
                let mut w = notify::recommended_watcher(
                    move |res: Result<notify::Event, notify::Error>| {
                        if res.is_ok() {
                            let _ = fs_tx.send(());
                        }
                    },
                )
                .expect("Failed to create file watcher");
                w.watch(Path::new(&ld), RecursiveMode::Recursive).ok();
                w.watch(Path::new(&rd), RecursiveMode::Recursive).ok();
                w
            };
            loop {
                std::thread::park();
            }
        });
    }

    // Poll for filesystem changes and reload
    let tabs_reload = open_tabs.clone();
    let ld_reload = left_dir.clone();
    let rd_reload = right_dir.clone();
    {
        let reload = reload_dir.clone();
        gtk4::glib::timeout_add_local(Duration::from_millis(500), move || {
            let mut changed = false;
            while fs_rx.try_recv().is_ok() {
                changed = true;
            }
            if changed && !SAVING.load(std::sync::atomic::Ordering::Relaxed) {
                reload();
                // Reload open file tabs
                for tab in tabs_reload.borrow().iter() {
                    reload_file_tab(tab, &ld_reload.borrow(), &rd_reload.borrow());
                }
            }
            gtk4::glib::ControlFlow::Continue
        });
    }

    // Window title
    let left_name = Path::new(left_dir.borrow().as_str())
        .file_name()
        .map_or_else(
            || left_dir.borrow().clone(),
            |n| n.to_string_lossy().into_owned(),
        );
    let right_name = Path::new(right_dir.borrow().as_str())
        .file_name()
        .map_or_else(
            || right_dir.borrow().clone(),
            |n| n.to_string_lossy().into_owned(),
        );
    let title = format!("{left_name} — {right_name}");

    let window = ApplicationWindow::builder()
        .application(app)
        .title(&title)
        .default_width(900)
        .default_height(600)
        .child(&notebook)
        .build();

    // Preferences action
    let win_actions = gio::SimpleActionGroup::new();
    {
        let action = gio::SimpleAction::new("prefs", None);
        let w = window.clone();
        let st = settings.clone();
        action.connect_activate(move |_, _| {
            show_preferences(&w, &st);
        });
        win_actions.add_action(&action);
    }
    window.insert_action_group("win", Some(&win_actions));
    window.insert_action_group("dir", Some(&dir_action_group));

    // Register keyboard accelerators
    if let Some(gtk_app) = window.application() {
        // Diff navigation (used by file diff tabs)
        gtk_app.set_accels_for_action("diff.prev-chunk", &["<Alt>Up", "<Ctrl>e"]);
        gtk_app.set_accels_for_action("diff.next-chunk", &["<Alt>Down", "<Ctrl>d"]);
        gtk_app.set_accels_for_action("diff.find", &["<Ctrl>f"]);
        gtk_app.set_accels_for_action("diff.find-replace", &["<Ctrl>h"]);
        gtk_app.set_accels_for_action("diff.find-next", &["F3"]);
        gtk_app.set_accels_for_action("diff.find-prev", &["<Shift>F3"]);
        gtk_app.set_accels_for_action("diff.go-to-line", &["<Ctrl>l"]);
        gtk_app.set_accels_for_action("diff.export-patch", &["<Ctrl><Shift>p"]);
        gtk_app.set_accels_for_action("win.prefs", &["<Ctrl>comma"]);
        // Directory copy actions
        gtk_app.set_accels_for_action("dir.folder-copy-left", &["<Alt>Left"]);
        gtk_app.set_accels_for_action("dir.folder-copy-right", &["<Alt>Right"]);
        gtk_app.set_accels_for_action("dir.folder-delete", &["Delete"]);
    }

    window.present();
    left_view.grab_focus();
}
