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

use std::cell::{Cell, RefCell};
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::fs;
use std::path::Path;
use std::rc::Rc;
use std::sync::mpsc;
use std::time::{Duration, SystemTime};

use chrono::{DateTime, Local};
use gtk4::gdk::Display;
use gtk4::gio;
use gtk4::gio::ListStore;
use gtk4::prelude::*;
use gtk4::{
    Adjustment, Application, ApplicationWindow, Box as GtkBox, Button, ColumnView,
    ColumnViewColumn, CssProvider, DrawingArea, GestureClick, Image, Label, ListItem, Notebook,
    Orientation, Paned, PolicyType, ScrolledWindow, SignalListItemFactory, SingleSelection,
    StringObject, TextBuffer, TextTag, TextView, TreeExpander, TreeListModel, TreeListRow,
    WrapMode,
};

use crate::myers::{self, DiffChunk, DiffTag};
use crate::CompareMode;

const CSS: &str = r"
.diff-changed { color: #729fcf; font-weight: bold; }
.diff-deleted { color: #f57900; }
.diff-inserted { color: #73d216; }
.diff-missing { color: #888a85; font-style: italic; }
.info-bar { background: #3584e4; padding: 8px 12px; }
.info-bar label { color: white; }
.chunk-label { font-size: 0.9em; }
.linked > button { min-width: 0; padding: 4px 8px; }
";

const SEP: char = '\x1f';

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
    chunks: Rc<RefCell<Vec<DiffChunk>>>,
    gutter: DrawingArea,
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
    for chunk in left_chunks {
        match chunk.tag {
            DiffTag::Equal | DiffTag::Insert => {}
            DiffTag::Replace => apply_line_tag(left_buf, "changed", chunk.start_a, chunk.end_a),
            DiffTag::Delete => apply_line_tag(left_buf, "deleted", chunk.start_a, chunk.end_a),
        }
    }

    // Right pane: b-side of right diff only
    for chunk in right_chunks {
        match chunk.tag {
            DiffTag::Equal | DiffTag::Delete => {}
            DiffTag::Replace => apply_line_tag(right_buf, "changed", chunk.start_b, chunk.end_b),
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

    tab.left_buf.set_text(&left_content);
    tab.right_buf.set_text(&right_content);

    let new_chunks = if left_content == right_content {
        Vec::new()
    } else {
        myers::diff_lines(&left_content, &right_content)
    };
    apply_diff_tags(&tab.left_buf, &tab.right_buf, &new_chunks);
    *tab.chunks.borrow_mut() = new_chunks;
    tab.gutter.queue_draw();
    // Buffer now matches disk — no unsaved changes
    tab.left_save.set_sensitive(false);
    tab.right_save.set_sensitive(false);
}

// ─── Helpers ───────────────────────────────────────────────────────────────

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

// ─── Directory scanning ────────────────────────────────────────────────────

fn read_dir_entries(dir: &Path) -> BTreeMap<String, DirMeta> {
    let mut map = BTreeMap::new();
    if let Ok(rd) = fs::read_dir(dir) {
        for entry in rd.filter_map(Result::ok) {
            let Ok(name) = entry.file_name().into_string() else {
                continue;
            };
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

    let left_entries = read_dir_entries(&left_dir);
    let right_entries = read_dir_entries(&right_dir);
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
                scan_level(left_root, right_root, &child_rel, children_map);
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
        let text = decode_field(&raw, field_idx);
        let status = decode_status(&raw);

        label.set_label(text);
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
}

fn make_diff_pane(buf: &TextBuffer, file_path: &Path, info: Option<&str>) -> DiffPane {
    let tv = TextView::with_buffer(buf);
    tv.set_editable(true);
    tv.set_monospace(true);
    tv.set_wrap_mode(WrapMode::None);
    tv.set_left_margin(4);

    // Line number gutter
    let line_gutter = DrawingArea::new();
    line_gutter.set_content_width(48);
    line_gutter.set_vexpand(true);
    {
        let tv_ref = tv.clone();
        let buf_ref = buf.clone();
        line_gutter.set_draw_func(move |_area, cr, width, _height| {
            draw_line_numbers(cr, width as f64, &tv_ref, &buf_ref);
        });
    }

    // Wrap text view + line gutter in a horizontal box
    let text_with_gutter = GtkBox::new(Orientation::Horizontal, 0);
    text_with_gutter.append(&line_gutter);
    text_with_gutter.append(&tv);
    tv.set_hexpand(true);

    let scroll = ScrolledWindow::builder()
        .min_content_width(360)
        .vexpand(true)
        .child(&text_with_gutter)
        .build();

    // Redraw line numbers on scroll and buffer changes
    {
        let g = line_gutter.clone();
        scroll
            .vadjustment()
            .connect_value_changed(move |_| g.queue_draw());
    }
    {
        let g = line_gutter.clone();
        buf.connect_changed(move |_| g.queue_draw());
    }

    let header = GtkBox::new(Orientation::Horizontal, 4);
    header.set_margin_start(4);
    header.set_margin_end(4);
    header.set_margin_top(2);
    header.set_margin_bottom(2);
    let save_btn = Button::from_icon_name("document-save-symbolic");
    save_btn.set_tooltip_text(Some("Save"));
    save_btn.set_sensitive(false);
    let path_label = Label::new(Some(&shortened_path(file_path)));
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
    // Compute position arithmetically: line_yrange can return stale values
    // after buffer modifications, but line 0's position and height are stable.
    if let Some(iter0) = buf.iter_at_line(0) {
        let (y0, line_h) = tv.line_yrange(&iter0);
        let buf_y = y0 as f64 + line as f64 * line_h as f64;
        let visible_y = buf_y - scroll.vadjustment().value();
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

fn draw_line_numbers(cr: &gtk4::cairo::Context, width: f64, tv: &TextView, buf: &TextBuffer) {
    let tv_height = tv.height() as f64;

    cr.set_source_rgb(0.93, 0.93, 0.93);
    cr.rectangle(0.0, 0.0, width, tv_height);
    let _ = cr.fill();

    cr.set_source_rgb(0.5, 0.5, 0.5);
    cr.set_font_size(11.0);

    let total_lines = buf.line_count();
    for line_num in 0..total_lines {
        if let Some(iter) = buf.iter_at_line(line_num) {
            let (y, h) = tv.line_yrange(&iter);
            // Convert buffer coords to widget coords
            let (_, widget_y) = tv.buffer_to_window_coords(gtk4::TextWindowType::Widget, 0, y);
            if widget_y + h < 0 {
                continue;
            }
            if widget_y > tv.height() {
                break;
            }
            let text = format!("{}", line_num + 1);
            let extents = cr.text_extents(&text).unwrap();
            let x = width - extents.width() - 6.0;
            let baseline = widget_y as f64 + f64::midpoint(h as f64, extents.height());
            cr.move_to(x, baseline);
            let _ = cr.show_text(&text);
        }
    }

    // Draw separator line on right edge
    cr.set_source_rgb(0.8, 0.8, 0.8);
    cr.set_line_width(1.0);
    cr.move_to(width - 0.5, 0.0);
    cr.line_to(width - 0.5, tv_height);
    let _ = cr.stroke();
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
    gutter: &DrawingArea,
) {
    let lt = left_buf.text(&left_buf.start_iter(), &left_buf.end_iter(), false);
    let rt = right_buf.text(&right_buf.start_iter(), &right_buf.end_iter(), false);

    left_buf.remove_all_tags(&left_buf.start_iter(), &left_buf.end_iter());
    right_buf.remove_all_tags(&right_buf.start_iter(), &right_buf.end_iter());

    let new_chunks = if lt == rt {
        Vec::new()
    } else {
        myers::diff_lines(&lt, &rt)
    };
    apply_diff_tags(left_buf, right_buf, &new_chunks);
    *chunks.borrow_mut() = new_chunks;
    gutter.queue_draw();
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

// ─── Shared diff view construction ─────────────────────────────────────────

struct DiffViewResult {
    widget: GtkBox,
    left_buf: TextBuffer,
    right_buf: TextBuffer,
    left_save: Button,
    right_save: Button,
    chunks: Rc<RefCell<Vec<DiffChunk>>>,
    gutter: DrawingArea,
    action_group: gio::SimpleActionGroup,
}

/// Build a complete diff view widget for two files.
/// Returns the top-level widget (toolbar + diff panes) and associated state.
fn build_diff_view(left_path: &Path, right_path: &Path) -> DiffViewResult {
    let left_content = fs::read_to_string(left_path).unwrap_or_default();
    let right_content = fs::read_to_string(right_path).unwrap_or_default();

    let left_buf = TextBuffer::new(None::<&gtk4::TextTagTable>);
    let right_buf = TextBuffer::new(None::<&gtk4::TextTagTable>);
    setup_diff_tags(&left_buf);
    setup_diff_tags(&right_buf);
    left_buf.set_text(&left_content);
    right_buf.set_text(&right_content);

    let identical = left_content == right_content;
    let diff_chunks = if identical {
        Vec::new()
    } else {
        myers::diff_lines(&left_content, &right_content)
    };
    apply_diff_tags(&left_buf, &right_buf, &diff_chunks);
    let chunks = Rc::new(RefCell::new(diff_chunks));

    let info_msg = if identical {
        Some("Files are identical")
    } else {
        None
    };

    let left_pane = make_diff_pane(&left_buf, left_path, info_msg);
    let right_pane = make_diff_pane(&right_buf, right_path, info_msg);

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

    // Re-diff on any buffer change
    {
        let pending = Rc::new(Cell::new(false));
        let connect_refresh = |buf: &TextBuffer| {
            let lb = left_buf.clone();
            let rb = right_buf.clone();
            let ch = chunks.clone();
            let g = gutter.clone();
            let p = pending.clone();
            buf.connect_changed(move |_| {
                if !p.get() {
                    p.set(true);
                    let lb = lb.clone();
                    let rb = rb.clone();
                    let ch = ch.clone();
                    let g = g.clone();
                    let p = p.clone();
                    gtk4::glib::idle_add_local_once(move || {
                        refresh_diff(&lb, &rb, &ch, &g);
                        p.set(false);
                    });
                }
            });
        };
        connect_refresh(&left_buf);
        connect_refresh(&right_buf);
    }

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

    let toolbar = GtkBox::new(Orientation::Horizontal, 8);
    toolbar.set_margin_start(6);
    toolbar.set_margin_end(6);
    toolbar.set_margin_top(4);
    toolbar.set_margin_bottom(4);
    toolbar.append(&nav_box);
    toolbar.append(&chunk_label);

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

    // Update chunk label when diff changes
    {
        let ch = chunks.clone();
        let cur = current_chunk.clone();
        let lbl = chunk_label.clone();
        let refresh_label = move || {
            cur.set(None);
            update_chunk_label(&lbl, &ch.borrow(), None);
        };
        let pending = Rc::new(Cell::new(false));
        let connect_label_refresh = |buf: &TextBuffer| {
            let r = Rc::new(refresh_label.clone());
            let p = pending.clone();
            buf.connect_changed(move |_| {
                if !p.get() {
                    p.set(true);
                    let r = r.clone();
                    let p = p.clone();
                    gtk4::glib::idle_add_local_once(move || {
                        r();
                        p.set(false);
                    });
                }
            });
        };
        connect_label_refresh(&left_buf);
        connect_label_refresh(&right_buf);
    }

    // Layout: toolbar + separator + [left pane | gutter | right pane]
    let diff_row = GtkBox::new(Orientation::Horizontal, 0);
    left_pane.container.set_hexpand(true);
    right_pane.container.set_hexpand(true);
    diff_row.append(&left_pane.container);
    diff_row.append(&gutter);
    diff_row.append(&right_pane.container);
    diff_row.set_vexpand(true);

    let widget = GtkBox::new(Orientation::Vertical, 0);
    widget.append(&toolbar);
    widget.append(&gtk4::Separator::new(Orientation::Horizontal));
    widget.append(&diff_row);

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

    DiffViewResult {
        widget,
        left_buf,
        right_buf,
        left_save: left_pane.save_btn,
        right_save: right_pane.save_btn,
        chunks,
        gutter,
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
) {
    let left_path = Path::new(left_dir).join(rel_path);
    let right_path = Path::new(right_dir).join(rel_path);

    let dv = build_diff_view(&left_path, &right_path);
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
        chunks: dv.chunks,
        gutter: dv.gutter,
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
    left_chunks: Rc<RefCell<Vec<DiffChunk>>>,
    right_chunks: Rc<RefCell<Vec<DiffChunk>>>,
    left_gutter: DrawingArea,
    right_gutter: DrawingArea,
    action_group: gio::SimpleActionGroup,
}

fn refresh_merge_diffs(
    left_buf: &TextBuffer,
    middle_buf: &TextBuffer,
    right_buf: &TextBuffer,
    left_chunks: &Rc<RefCell<Vec<DiffChunk>>>,
    right_chunks: &Rc<RefCell<Vec<DiffChunk>>>,
    left_gutter: &DrawingArea,
    right_gutter: &DrawingArea,
) {
    let lt = left_buf.text(&left_buf.start_iter(), &left_buf.end_iter(), false);
    let mt = middle_buf.text(&middle_buf.start_iter(), &middle_buf.end_iter(), false);
    let rt = right_buf.text(&right_buf.start_iter(), &right_buf.end_iter(), false);

    left_buf.remove_all_tags(&left_buf.start_iter(), &left_buf.end_iter());
    middle_buf.remove_all_tags(&middle_buf.start_iter(), &middle_buf.end_iter());
    right_buf.remove_all_tags(&right_buf.start_iter(), &right_buf.end_iter());

    let new_left = if lt == mt {
        Vec::new()
    } else {
        myers::diff_lines(&lt, &mt)
    };
    let new_right = if mt == rt {
        Vec::new()
    } else {
        myers::diff_lines(&mt, &rt)
    };

    apply_merge_tags(left_buf, middle_buf, right_buf, &new_left, &new_right);

    *left_chunks.borrow_mut() = new_left;
    *right_chunks.borrow_mut() = new_right;
    left_gutter.queue_draw();
    right_gutter.queue_draw();
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
) -> MergeViewResult {
    let left_content = fs::read_to_string(left_path).unwrap_or_default();
    // When --output is set, the middle pane shows the merged file (with conflict markers),
    // not the base. This matches git mergetool semantics: you edit $MERGED, not $BASE.
    let middle_display_path = output.unwrap_or(middle_path);
    let middle_content = fs::read_to_string(middle_display_path).unwrap_or_default();
    let right_content = fs::read_to_string(right_path).unwrap_or_default();

    let left_buf = TextBuffer::new(None::<&gtk4::TextTagTable>);
    let middle_buf = TextBuffer::new(None::<&gtk4::TextTagTable>);
    let right_buf = TextBuffer::new(None::<&gtk4::TextTagTable>);
    setup_diff_tags(&left_buf);
    setup_diff_tags(&middle_buf);
    setup_diff_tags(&right_buf);
    left_buf.set_text(&left_content);
    middle_buf.set_text(&middle_content);
    right_buf.set_text(&right_content);

    // Two pairwise diffs
    let lc = if left_content == middle_content {
        Vec::new()
    } else {
        myers::diff_lines(&left_content, &middle_content)
    };
    let rc = if middle_content == right_content {
        Vec::new()
    } else {
        myers::diff_lines(&middle_content, &right_content)
    };

    apply_merge_tags(&left_buf, &middle_buf, &right_buf, &lc, &rc);

    let left_chunks = Rc::new(RefCell::new(lc));
    let right_chunks = Rc::new(RefCell::new(rc));

    let left_pane = make_diff_pane(&left_buf, left_path, None);
    let middle_pane = make_diff_pane(&middle_buf, middle_display_path, None);
    let right_pane = make_diff_pane(&right_buf, right_path, None);

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

    // Re-diff on any buffer change (debounced)
    {
        let pending = Rc::new(Cell::new(false));
        let connect_refresh = |buf: &TextBuffer| {
            let lb = left_buf.clone();
            let mb = middle_buf.clone();
            let rb = right_buf.clone();
            let lch = left_chunks.clone();
            let rch = right_chunks.clone();
            let lg = left_gutter.clone();
            let rg = right_gutter.clone();
            let p = pending.clone();
            buf.connect_changed(move |_| {
                if !p.get() {
                    p.set(true);
                    let lb = lb.clone();
                    let mb = mb.clone();
                    let rb = rb.clone();
                    let lch = lch.clone();
                    let rch = rch.clone();
                    let lg = lg.clone();
                    let rg = rg.clone();
                    let p = p.clone();
                    gtk4::glib::idle_add_local_once(move || {
                        refresh_merge_diffs(&lb, &mb, &rb, &lch, &rch, &lg, &rg);
                        p.set(false);
                    });
                }
            });
        };
        connect_refresh(&left_buf);
        connect_refresh(&middle_buf);
        connect_refresh(&right_buf);
    }

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

    let toolbar = GtkBox::new(Orientation::Horizontal, 8);
    toolbar.set_margin_start(6);
    toolbar.set_margin_end(6);
    toolbar.set_margin_top(4);
    toolbar.set_margin_bottom(4);
    toolbar.append(&nav_box);
    toolbar.append(&chunk_label);

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

    // Update chunk label when diff changes
    {
        let lch = left_chunks.clone();
        let rch = right_chunks.clone();
        let cur = current_chunk.clone();
        let lbl = chunk_label.clone();
        let upd = update_merge_label;
        let refresh_label = move || {
            cur.set(None);
            upd(&lbl, &lch.borrow(), &rch.borrow(), None);
        };
        let pending = Rc::new(Cell::new(false));
        let connect_label_refresh = |buf: &TextBuffer| {
            let r = Rc::new(refresh_label.clone());
            let p = pending.clone();
            buf.connect_changed(move |_| {
                if !p.get() {
                    p.set(true);
                    let r = r.clone();
                    let p = p.clone();
                    gtk4::glib::idle_add_local_once(move || {
                        r();
                        p.set(false);
                    });
                }
            });
        };
        connect_label_refresh(&left_buf);
        connect_label_refresh(&middle_buf);
        connect_label_refresh(&right_buf);
    }

    // Layout: [left pane | left_gutter | middle pane | right_gutter | right pane]
    let diff_row = GtkBox::new(Orientation::Horizontal, 0);
    left_pane.container.set_hexpand(true);
    middle_pane.container.set_hexpand(true);
    right_pane.container.set_hexpand(true);
    diff_row.append(&left_pane.container);
    diff_row.append(&left_gutter);
    diff_row.append(&middle_pane.container);
    diff_row.append(&right_gutter);
    diff_row.append(&right_pane.container);
    diff_row.set_vexpand(true);

    let widget = GtkBox::new(Orientation::Vertical, 0);
    widget.append(&toolbar);
    widget.append(&gtk4::Separator::new(Orientation::Horizontal));
    widget.append(&diff_row);

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

    MergeViewResult {
        widget,
        left_buf,
        middle_buf,
        right_buf,
        middle_save: middle_pane.save_btn,
        left_chunks,
        right_chunks,
        left_gutter,
        right_gutter,
        action_group,
    }
}

fn build_merge_window(
    app: &Application,
    left_path: std::path::PathBuf,
    middle_path: std::path::PathBuf,
    right_path: std::path::PathBuf,
    output: Option<std::path::PathBuf>,
) {
    let mv = build_merge_view(
        &left_path,
        &middle_path,
        &right_path,
        output.as_deref(),
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
        if let Some(toolbar) = mv.widget.first_child() {
            if let Some(toolbar_box) = toolbar.downcast_ref::<GtkBox>() {
                toolbar_box.append(&save_btn);
            }
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
        let lch = mv.left_chunks.clone();
        let rch = mv.right_chunks.clone();
        let lg = mv.left_gutter.clone();
        let rg = mv.right_gutter.clone();
        let lp = left_path.clone();
        let mp = output.clone().unwrap_or_else(|| middle_path.clone());
        let rp = right_path.clone();
        let m_save = mv.middle_save.clone();
        gtk4::glib::timeout_add_local(Duration::from_millis(500), move || {
            let mut changed = false;
            while fs_rx.try_recv().is_ok() {
                changed = true;
            }
            if changed && !SAVING.load(std::sync::atomic::Ordering::Relaxed) {
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
                    lb.set_text(&left_content);
                    mb.set_text(&middle_content);
                    rb.set_text(&right_content);
                    refresh_merge_diffs(&lb, &mb, &rb, &lch, &rch, &lg, &rg);
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

    if let Some(gtk_app) = window.application() {
        gtk_app.set_accels_for_action("diff.prev-chunk", &["<Alt>Up", "<Ctrl>e"]);
        gtk_app.set_accels_for_action("diff.next-chunk", &["<Alt>Down", "<Ctrl>d"]);
    }

    window.present();
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

        match mode {
            CompareMode::Dirs { left, right } => build_dir_window(app, left, right),
            CompareMode::Files { left, right } => build_file_window(app, left, right),
            CompareMode::Merge {
                left,
                middle,
                right,
                output,
            } => build_merge_window(app, left, middle, right, output),
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
) {
    let dv = build_diff_view(&left_path, &right_path);

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
        let ch = dv.chunks.clone();
        let g = dv.gutter.clone();
        let lp = left_path.clone();
        let rp = right_path.clone();
        let l_save = dv.left_save.clone();
        let r_save = dv.right_save.clone();
        gtk4::glib::timeout_add_local(Duration::from_millis(500), move || {
            let mut changed = false;
            while fs_rx.try_recv().is_ok() {
                changed = true;
            }
            if changed && !SAVING.load(std::sync::atomic::Ordering::Relaxed) {
                let left_content = fs::read_to_string(&lp).unwrap_or_default();
                let right_content = fs::read_to_string(&rp).unwrap_or_default();

                let cur_left = lb.text(&lb.start_iter(), &lb.end_iter(), false);
                let cur_right = rb.text(&rb.start_iter(), &rb.end_iter(), false);

                if cur_left.as_str() != left_content || cur_right.as_str() != right_content {
                    lb.set_text(&left_content);
                    rb.set_text(&right_content);
                    let new_chunks = if left_content == right_content {
                        Vec::new()
                    } else {
                        myers::diff_lines(&left_content, &right_content)
                    };
                    apply_diff_tags(&lb, &rb, &new_chunks);
                    *ch.borrow_mut() = new_chunks;
                    g.queue_draw();
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

    // Bind keyboard accelerators
    if let Some(gtk_app) = window.application() {
        gtk_app.set_accels_for_action("diff.prev-chunk", &["<Alt>Up", "<Ctrl>e"]);
        gtk_app.set_accels_for_action("diff.next-chunk", &["<Alt>Down", "<Ctrl>d"]);
    }

    window.present();
}

// ─── Directory comparison window ───────────────────────────────────────────

fn build_dir_window(
    app: &Application,
    left_dir: std::path::PathBuf,
    right_dir: std::path::PathBuf,
) {
    let left_dir = Rc::new(left_dir.to_string_lossy().into_owned());
    let right_dir = Rc::new(right_dir.to_string_lossy().into_owned());

    // Build tree data (Rc<RefCell<…>> so the watcher callback can rebuild)
    let children_map = Rc::new(RefCell::new(HashMap::new()));
    let root_store = ListStore::new::<StringObject>();
    {
        let (store, _) = scan_level(
            Path::new(left_dir.as_str()),
            Path::new(right_dir.as_str()),
            "",
            &mut children_map.borrow_mut(),
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

    // ── Left pane ──────────────────────────────────────────────────
    let left_sel = SingleSelection::new(Some(tree_model.clone()));
    let left_view = ColumnView::new(Some(left_sel));
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
    let right_sel = SingleSelection::new(Some(tree_model.clone()));
    let right_view = ColumnView::new(Some(right_sel));
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

    // ── Notebook (tabs) ────────────────────────────────────────────
    let notebook = Notebook::new();
    notebook.set_scrollable(true);
    notebook.append_page(&dir_paned, Some(&Label::new(Some("Directory"))));

    // Open file tabs tracking
    let open_tabs: Rc<RefCell<Vec<FileTab>>> = Rc::new(RefCell::new(Vec::new()));

    // Activate handlers — double-click a file row to open diff in new tab
    {
        let nb = notebook.clone();
        let tm = tree_model.clone();
        let tabs = open_tabs.clone();
        let ld = left_dir.clone();
        let rd = right_dir.clone();
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
                        &ld,
                        &rd,
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
                        &ld,
                        &rd,
                    );
                }
            }
        });
    }

    // ── File watcher ───────────────────────────────────────────────
    let (fs_tx, fs_rx) = mpsc::channel::<()>();
    {
        let ld = left_dir.to_string();
        let rd = right_dir.to_string();
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
    let cm_reload = children_map.clone();
    let rs_reload = root_store.clone();
    let tabs_reload = open_tabs.clone();
    let ld_reload = left_dir.clone();
    let rd_reload = right_dir.clone();
    gtk4::glib::timeout_add_local(Duration::from_millis(500), move || {
        let mut changed = false;
        while fs_rx.try_recv().is_ok() {
            changed = true;
        }
        if changed && !SAVING.load(std::sync::atomic::Ordering::Relaxed) {
            // Build new tree into a temporary map (not inside borrow_mut)
            let mut new_map = HashMap::new();
            let (new_store, _) = scan_level(
                Path::new(ld_reload.as_str()),
                Path::new(rd_reload.as_str()),
                "",
                &mut new_map,
            );
            // Replace children_map, then drop the borrow before touching the store.
            // Appending to root_store triggers TreeListModel's create_func which
            // borrows children_map immutably — so we must not hold a mutable borrow.
            *cm_reload.borrow_mut() = new_map;
            rs_reload.remove_all();
            for i in 0..new_store.n_items() {
                if let Some(obj) = new_store.item(i) {
                    rs_reload.append(&obj.downcast::<StringObject>().unwrap());
                }
            }
            // Reload open file tabs
            for tab in tabs_reload.borrow().iter() {
                reload_file_tab(tab, &ld_reload, &rd_reload);
            }
        }
        gtk4::glib::ControlFlow::Continue
    });

    // Window title
    let left_name = Path::new(left_dir.as_str()).file_name().map_or_else(
        || left_dir.to_string(),
        |n| n.to_string_lossy().into_owned(),
    );
    let right_name = Path::new(right_dir.as_str()).file_name().map_or_else(
        || right_dir.to_string(),
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

    // Register keyboard accelerators for diff navigation (used by file diff tabs)
    if let Some(gtk_app) = window.application() {
        gtk_app.set_accels_for_action("diff.prev-chunk", &["<Alt>Up", "<Ctrl>e"]);
        gtk_app.set_accels_for_action("diff.next-chunk", &["<Alt>Down", "<Ctrl>d"]);
    }

    window.present();
}
