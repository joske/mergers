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
    Application, ApplicationWindow, Box as GtkBox, Button, ColumnView, ColumnViewColumn,
    CssProvider, DrawingArea, GestureClick, Image, Label, ListItem, Notebook, Orientation, Paned,
    PolicyType, ScrolledWindow, SignalListItemFactory, SingleSelection, StringObject, TextBuffer,
    TextTag, TextView, TreeExpander, TreeListModel, TreeListRow,
};

use crate::myers::{self, DiffChunk, DiffTag};

const LEFT_DIR: &str = "/home/jos/tmp/cmp1";
const RIGHT_DIR: &str = "/home/jos/tmp/cmp2";

const CSS: &str = r#"
.diff-changed { color: #729fcf; font-weight: bold; }
.diff-deleted { color: #f57900; }
.diff-inserted { color: #73d216; }
.diff-missing { color: #888a85; font-style: italic; }
.info-bar { background: #3584e4; padding: 8px 12px; }
.info-bar label { color: white; }
"#;

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

fn reload_file_tab(tab: &FileTab) {
    let left_content =
        fs::read_to_string(Path::new(LEFT_DIR).join(&tab.rel_path)).unwrap_or_default();
    let right_content =
        fs::read_to_string(Path::new(RIGHT_DIR).join(&tab.rel_path)).unwrap_or_default();

    // Only reset buffers if the on-disk content actually differs from what's in the buffer
    let cur_left = tab
        .left_buf
        .text(&tab.left_buf.start_iter(), &tab.left_buf.end_iter(), false);
    let cur_right = tab
        .right_buf
        .text(&tab.right_buf.start_iter(), &tab.right_buf.end_iter(), false);

    if cur_left.as_str() == left_content && cur_right.as_str() == right_content {
        return; // nothing changed on disk vs buffer
    }

    tab.left_buf.set_text(&left_content);
    tab.right_buf.set_text(&right_content);

    let new_chunks = if left_content != right_content {
        myers::diff_lines(&left_content, &right_content)
    } else {
        Vec::new()
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
        for entry in rd.filter_map(|e| e.ok()) {
            let name = match entry.file_name().into_string() {
                Ok(n) => n,
                Err(_) => continue,
            };
            let meta = entry.metadata().ok();
            let is_dir = entry.path().is_dir();
            map.insert(
                name,
                DirMeta {
                    size: meta.as_ref().map(|m| m.len()),
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
/// Returns (store_for_this_level, aggregate_status).
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
            let is_dir = left_entries.get(*n).map(|m| m.is_dir).unwrap_or(false)
                || right_entries.get(*n).map(|m| m.is_dir).unwrap_or(false);
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

/// Name column: TreeExpander → Box → [Icon, Label]
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
    let scroll = ScrolledWindow::builder()
        .min_content_width(360)
        .vexpand(true)
        .child(&tv)
        .build();

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
            _ => continue,
        };

        // Filled band
        cr.set_source_rgba(r, g, b, 0.3);
        cr.move_to(0.0, lt);
        cr.curve_to(width * 0.5, lt, width * 0.5, rt, width, rt);
        cr.line_to(width, rb);
        cr.curve_to(width * 0.5, rb, width * 0.5, lb, 0.0, lb);
        cr.close_path();
        let _ = cr.fill();

        // Outline
        cr.set_source_rgba(r, g, b, 0.7);
        cr.set_line_width(1.0);
        cr.move_to(0.0, lt);
        cr.curve_to(width * 0.5, lt, width * 0.5, rt, width, rt);
        let _ = cr.stroke();
        cr.move_to(0.0, lb);
        cr.curve_to(width * 0.5, lb, width * 0.5, rb, width, rb);
        let _ = cr.stroke();

        // Arrows
        let mid_y = (lt + lb + rt + rb) / 4.0;
        draw_arrow(cr, width * 0.25, mid_y, true, r, g, b);
        draw_arrow(cr, width * 0.75, mid_y, false, r, g, b);
    }
}

fn draw_arrow(
    cr: &gtk4::cairo::Context,
    cx: f64,
    cy: f64,
    right: bool,
    r: f64,
    g: f64,
    b: f64,
) {
    let radius = 9.0;
    cr.set_source_rgba(r, g, b, 0.85);
    cr.arc(cx, cy, radius, 0.0, 2.0 * std::f64::consts::PI);
    let _ = cr.fill();

    cr.set_source_rgba(1.0, 1.0, 1.0, 1.0);
    cr.set_line_width(2.0);
    let d = 4.0;
    if right {
        cr.move_to(cx - d, cy - d);
        cr.line_to(cx + d, cy);
        cr.line_to(cx - d, cy + d);
    } else {
        cr.move_to(cx + d, cy - d);
        cr.line_to(cx - d, cy);
        cr.line_to(cx + d, cy + d);
    }
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
        buf.iter_at_line(end_line as i32)
            .unwrap_or(buf.end_iter())
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

    let new_chunks = if lt != rt {
        myers::diff_lines(&lt, &rt)
    } else {
        Vec::new()
    };
    apply_diff_tags(left_buf, right_buf, &new_chunks);
    *chunks.borrow_mut() = new_chunks;
    gutter.queue_draw();
}

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
        let mid_y = (lt + lb + rt + rb) / 4.0;

        let hit = 12.0_f64;
        // → arrow: copy left to right
        if (x - width * 0.25).powi(2) + (y - mid_y).powi(2) < hit * hit {
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
        // ← arrow: copy right to left
        if (x - width * 0.75).powi(2) + (y - mid_y).powi(2) < hit * hit {
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

// ─── Open file diff in new tab ─────────────────────────────────────────────

fn open_file_diff(
    notebook: &Notebook,
    rel_path: &str,
    status: &str,
    open_tabs: &Rc<RefCell<Vec<FileTab>>>,
) {
    let left_path = Path::new(LEFT_DIR).join(rel_path);
    let right_path = Path::new(RIGHT_DIR).join(rel_path);

    let left_content = fs::read_to_string(&left_path).unwrap_or_default();
    let right_content = fs::read_to_string(&right_path).unwrap_or_default();

    let left_buf = TextBuffer::new(None::<&gtk4::TextTagTable>);
    let right_buf = TextBuffer::new(None::<&gtk4::TextTagTable>);
    setup_diff_tags(&left_buf);
    setup_diff_tags(&right_buf);
    left_buf.set_text(&left_content);
    right_buf.set_text(&right_content);

    let diff_chunks = if status == "D" {
        myers::diff_lines(&left_content, &right_content)
    } else {
        Vec::new()
    };
    apply_diff_tags(&left_buf, &right_buf, &diff_chunks);
    let chunks = Rc::new(RefCell::new(diff_chunks));

    let info_msg = if status == "S" {
        Some("Files are identical")
    } else {
        None
    };

    let left_pane = make_diff_pane(&left_buf, &left_path, info_msg);
    let right_pane = make_diff_pane(&right_buf, &right_path, info_msg);

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

    // Re-diff on any buffer change (undo, redo, typing, merge arrows)
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

    // Redraw gutter on scroll
    {
        let g = gutter.clone();
        left_pane
            .scroll
            .vadjustment()
            .connect_value_changed(move |_| g.queue_draw());
    }
    {
        let g = gutter.clone();
        right_pane
            .scroll
            .vadjustment()
            .connect_value_changed(move |_| g.queue_draw());
    }

    // Layout: [left pane] [gutter] [right pane]
    let content = GtkBox::new(Orientation::Horizontal, 0);
    left_pane.container.set_hexpand(true);
    right_pane.container.set_hexpand(true);
    content.append(&left_pane.container);
    content.append(&gutter);
    content.append(&right_pane.container);
    content.set_vexpand(true);

    // Track tab
    let tab_id = NEXT_TAB_ID.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
    open_tabs.borrow_mut().push(FileTab {
        id: tab_id,
        rel_path: rel_path.to_string(),
        left_buf: left_buf.clone(),
        right_buf: right_buf.clone(),
        left_save: left_pane.save_btn.clone(),
        right_save: right_pane.save_btn.clone(),
        chunks: chunks.clone(),
        gutter: gutter.clone(),
    });

    // Tab label
    let file_name = Path::new(rel_path)
        .file_name()
        .map(|n| n.to_string_lossy().into_owned())
        .unwrap_or_else(|| rel_path.to_string());
    let left_dir_name = Path::new(LEFT_DIR)
        .file_name()
        .map(|n| n.to_string_lossy().into_owned())
        .unwrap_or_default();
    let right_dir_name = Path::new(RIGHT_DIR)
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

    let page_num = notebook.append_page(&content, Some(&tab_label_box));
    notebook.set_current_page(Some(page_num));

    let nb = notebook.clone();
    let c = content.clone();
    let tabs = open_tabs.clone();
    close_btn.connect_clicked(move |_| {
        if let Some(n) = nb.page_num(&c) {
            nb.remove_page(Some(n));
        }
        tabs.borrow_mut().retain(|t| t.id != tab_id);
    });
}

// ─── Main UI ───────────────────────────────────────────────────────────────

pub(crate) fn build_ui(application: &Application) {
    application.connect_activate(|app| {
        // Load CSS
        let provider = CssProvider::new();
        provider.load_from_string(CSS);
        gtk4::style_context_add_provider_for_display(
            &Display::default().unwrap(),
            &provider,
            gtk4::STYLE_PROVIDER_PRIORITY_APPLICATION,
        );

        // Build tree data (Rc<RefCell<…>> so the watcher callback can rebuild)
        let children_map = Rc::new(RefCell::new(HashMap::new()));
        let root_store = ListStore::new::<StringObject>();
        {
            let (store, _) = scan_level(
                Path::new(LEFT_DIR),
                Path::new(RIGHT_DIR),
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
        let tree_model =
            TreeListModel::new(root_store.clone(), false, false, move |item| {
                let obj = item.downcast_ref::<StringObject>()?;
                let raw = obj.string();
                if decode_is_dir(&raw) {
                    let rel = decode_rel_path(&raw);
                    cm.borrow()
                        .get(rel)
                        .cloned()
                        .map(|s| s.upcast::<gio::ListModel>())
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
            let col = ColumnViewColumn::new(
                Some("Modification time"),
                Some(make_field_factory(true, 5)),
            );
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
                        );
                    }
                }
            });
        }
        {
            let nb = notebook.clone();
            let tm = tree_model.clone();
            let tabs = open_tabs.clone();
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
                        );
                    }
                }
            });
        }

        // ── File watcher ───────────────────────────────────────────────
        let (fs_tx, fs_rx) = mpsc::channel::<()>();
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
                w.watch(Path::new(LEFT_DIR), RecursiveMode::Recursive).ok();
                w.watch(Path::new(RIGHT_DIR), RecursiveMode::Recursive).ok();
                w
            };
            loop {
                std::thread::park();
            }
        });

        // Poll for filesystem changes and reload
        let cm_reload = children_map.clone();
        let rs_reload = root_store.clone();
        let tabs_reload = open_tabs.clone();
        gtk4::glib::timeout_add_local(Duration::from_millis(500), move || {
            let mut changed = false;
            while fs_rx.try_recv().is_ok() {
                changed = true;
            }
            if changed && !SAVING.load(std::sync::atomic::Ordering::Relaxed) {
                // Build new tree into a temporary map (not inside borrow_mut)
                let mut new_map = HashMap::new();
                let (new_store, _) = scan_level(
                    Path::new(LEFT_DIR),
                    Path::new(RIGHT_DIR),
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
                    reload_file_tab(tab);
                }
            }
            gtk4::glib::ControlFlow::Continue
        });

        // Window
        let window = ApplicationWindow::builder()
            .application(app)
            .title("Meld-rs")
            .default_width(900)
            .default_height(600)
            .child(&notebook)
            .build();
        window.present();
    });
}
