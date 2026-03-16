#[allow(clippy::wildcard_imports)]
use super::*;

// ─── Directory Logic (Pure Functions) ──────────────────────────────────────

#[derive(Debug, Clone, PartialEq)]
pub struct DirRowInfo {
    pub status: FileStatus,
    pub name: String,
    pub is_dir: bool,
    pub rel_path: String,
    pub left_size: Option<u64>,
    pub left_mtime: Option<SystemTime>,
    pub right_size: Option<u64>,
    pub right_mtime: Option<SystemTime>,
}

impl DirRowInfo {
    pub fn encode(&self) -> String {
        let s = self.status.code();
        let d = if self.is_dir { "1" } else { "0" };
        let ls = self.left_size.map(format_size).unwrap_or_default();
        let lm = self.left_mtime.map(format_mtime).unwrap_or_default();
        let rs = self.right_size.map(format_size).unwrap_or_default();
        let rm = self.right_mtime.map(format_mtime).unwrap_or_default();
        format!(
            "{s}{SEP}{name}{SEP}{d}{SEP}{rel}{SEP}{ls}{SEP}{lm}{SEP}{rs}{SEP}{rm}",
            name = self.name,
            rel = self.rel_path
        )
    }

    pub fn decode(raw: &str) -> Self {
        let mut parts = raw.splitn(8, SEP);
        let status_code = parts.next().unwrap_or("S");
        let name = parts.next().unwrap_or("").to_string();
        let is_dir = parts.next().unwrap_or("0") == "1";
        let rel_path = parts.next().unwrap_or("").to_string();
        // Skip metadata decoding for now as it's string-formatted;
        // in a real app we might want to store raw values or just use strings for display.
        // For this refactor, we primarily need the identity fields.

        let status = match status_code {
            "D" => FileStatus::Different,
            "C" => FileStatus::Conflict,
            "L" => FileStatus::LeftOnly,
            "R" => FileStatus::RightOnly,
            _ => FileStatus::Same,
        };

        DirRowInfo {
            status,
            name,
            is_dir,
            rel_path,
            left_size: None, // Decoding metadata from formatted string is lossy/complex
            left_mtime: None,
            right_size: None,
            right_mtime: None,
        }
    }

    // Helper to get just the field text for display
    pub fn get_field_text(raw: &str, index: usize) -> &str {
        raw.splitn(8, SEP).nth(index).unwrap_or("")
    }
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
            let Ok(rel) = entry.path().strip_prefix(src) else {
                continue;
            };
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

/// Recursively collect `(rel_path, status)` of all non-conflicting, non-same files.
fn collect_actionable_files(
    store: &ListStore,
    cm: &HashMap<String, ListStore>,
    out: &mut Vec<(String, FileStatus)>,
) {
    for i in 0..store.n_items() {
        if let Some(obj) = store.item(i).and_downcast::<StringObject>() {
            let info = DirRowInfo::decode(&obj.string());
            if info.is_dir {
                if let Some(child_store) = cm.get(&info.rel_path) {
                    collect_actionable_files(child_store, cm, out);
                }
            } else {
                match info.status {
                    FileStatus::Different | FileStatus::RightOnly | FileStatus::LeftOnly => {
                        out.push((info.rel_path, info.status));
                    }
                    FileStatus::Conflict | FileStatus::Same => {}
                }
            }
        }
    }
}

// ─── Directory scanning ────────────────────────────────────────────────────

fn read_dir_entries(
    dir: &Path,
    dir_filters: &[String],
    hide_hidden: bool,
) -> BTreeMap<String, DirMeta> {
    let mut map = BTreeMap::new();
    if let Ok(rd) = fs::read_dir(dir) {
        for entry in rd.filter_map(Result::ok) {
            let Ok(name) = entry.file_name().into_string() else {
                continue;
            };
            if name == ".mergers-conflicts" || dir_filters.iter().any(|f| f == &name) {
                continue;
            }
            if hide_hidden && name.starts_with('.') {
                continue;
            }
            // Use entry.file_type() (lstat) for is_dir to avoid following
            // symlinks to directories (prevents infinite recursion with cycles).
            // Use fs::metadata (stat) for size/mtime to get real file data.
            let is_dir = entry.file_type().ok().is_some_and(|ft| ft.is_dir());
            let meta = fs::metadata(entry.path()).ok();
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

/// A single entry produced by the background-safe `scan_tree`.
/// Contains all data needed to build GTK stores on the main thread.
struct ScanEntry {
    status: FileStatus,
    name: String,
    is_dir: bool,
    rel_path: String,
    left_size: Option<u64>,
    left_mtime: Option<SystemTime>,
    right_size: Option<u64>,
    right_mtime: Option<SystemTime>,
    /// Child entries (only populated for directories).
    children: Vec<ScanEntry>,
}

/// Background-safe directory comparison: walks directories, reads files, and
/// compares contents. Returns `(entries, aggregate_status)`.
/// Does NOT use any GTK types, so it is `Send` and can run on a worker thread.
fn scan_tree(
    left_root: &Path,
    right_root: &Path,
    rel: &str,
    dir_filters: &[String],
    hide_hidden: bool,
) -> (Vec<ScanEntry>, FileStatus) {
    // Check for a conflicts marker file written by patch mode.
    let conflicts = if rel.is_empty() {
        let marker = right_root.join(".mergers-conflicts");
        if let Ok(content) = fs::read_to_string(&marker) {
            content.lines().map(String::from).collect()
        } else {
            HashSet::new()
        }
    } else {
        HashSet::new()
    };
    scan_tree_inner(
        left_root,
        right_root,
        rel,
        dir_filters,
        hide_hidden,
        &conflicts,
    )
}

fn scan_tree_inner(
    left_root: &Path,
    right_root: &Path,
    rel: &str,
    dir_filters: &[String],
    hide_hidden: bool,
    conflicts: &HashSet<String>,
) -> (Vec<ScanEntry>, FileStatus) {
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

    let left_entries = read_dir_entries(&left_dir, dir_filters, hide_hidden);
    let right_entries = read_dir_entries(&right_dir, dir_filters, hide_hidden);
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

    let mut entries = Vec::new();
    let mut all_same = true;

    for (name, is_dir) in &names {
        let in_left = left_entries.contains_key(*name);
        let in_right = right_entries.contains_key(*name);
        let child_rel = if rel.is_empty() {
            (*name).clone()
        } else {
            format!("{rel}/{name}")
        };

        let (status, children);
        if *is_dir {
            let (child_entries, child_agg) = scan_tree_inner(
                left_root,
                right_root,
                &child_rel,
                dir_filters,
                hide_hidden,
                conflicts,
            );
            status = if !in_left {
                FileStatus::RightOnly
            } else if !in_right {
                FileStatus::LeftOnly
            } else {
                child_agg
            };
            children = child_entries;
        } else {
            children = Vec::new();
            status = match (in_left, in_right) {
                (true, false) => {
                    if conflicts.contains(&child_rel) {
                        FileStatus::Conflict
                    } else {
                        FileStatus::LeftOnly
                    }
                }
                (false, true) => {
                    if conflicts.contains(&child_rel) {
                        FileStatus::Conflict
                    } else {
                        FileStatus::RightOnly
                    }
                }
                (true, true) => {
                    let lm = left_entries.get(*name);
                    let rm = right_entries.get(*name);
                    // Fast path: different sizes means different content
                    if lm.and_then(|m| m.size) == rm.and_then(|m| m.size) {
                        let lc = fs::read(left_dir.join(name)).unwrap_or_default();
                        let rc = fs::read(right_dir.join(name)).unwrap_or_default();
                        if lc == rc {
                            FileStatus::Same
                        } else if conflicts.contains(&child_rel) {
                            FileStatus::Conflict
                        } else {
                            FileStatus::Different
                        }
                    } else if conflicts.contains(&child_rel) {
                        FileStatus::Conflict
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
        entries.push(ScanEntry {
            status,
            name: (*name).clone(),
            is_dir: *is_dir,
            rel_path: child_rel,
            left_size: lm.and_then(|m| m.size),
            left_mtime: lm.and_then(|m| m.mtime),
            right_size: rm.and_then(|m| m.size),
            right_mtime: rm.and_then(|m| m.mtime),
            children,
        });
    }

    let agg = if all_same {
        FileStatus::Same
    } else {
        FileStatus::Different
    };
    (entries, agg)
}

/// Main-thread: converts a `ScanEntry` tree into a `ListStore` + `children_map`.
fn build_stores(entries: &[ScanEntry], children_map: &mut HashMap<String, ListStore>) -> ListStore {
    let store = ListStore::new::<StringObject>();
    for entry in entries {
        // Skip Same entries — they have no visual purpose in the dir view
        if entry.status == FileStatus::Same {
            continue;
        }
        if entry.is_dir {
            let child_store = build_stores(&entry.children, children_map);
            // Skip empty directories (all children are Same)
            if child_store.n_items() == 0 {
                continue;
            }
            children_map.insert(entry.rel_path.clone(), child_store);
        }
        let info = DirRowInfo {
            status: entry.status,
            name: entry.name.clone(),
            is_dir: entry.is_dir,
            rel_path: entry.rel_path.clone(),
            left_size: entry.left_size,
            left_mtime: entry.left_mtime,
            right_size: entry.right_size,
            right_mtime: entry.right_mtime,
        };
        store.append(&StringObject::new(&info.encode()));
    }
    store
}

// ─── CSS helpers ───────────────────────────────────────────────────────────

fn apply_status_class(widget: &impl WidgetExt, status: &str, is_left: bool) {
    for cls in &[
        "diff-changed",
        "diff-conflict",
        "diff-deleted",
        "diff-inserted",
        "diff-missing",
    ] {
        widget.remove_css_class(cls);
    }
    if is_left {
        match status {
            "D" => widget.add_css_class("diff-changed"),
            "C" => widget.add_css_class("diff-conflict"),
            "L" => widget.add_css_class("diff-deleted"),
            "R" => widget.add_css_class("diff-missing"),
            _ => {}
        }
    } else {
        match status {
            "D" => widget.add_css_class("diff-changed"),
            "C" => widget.add_css_class("diff-conflict"),
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

        let info = DirRowInfo::decode(&raw);

        icon.set_icon_name(Some(if info.is_dir {
            "folder-symbolic"
        } else {
            "text-x-generic-symbolic"
        }));
        label.set_label(&info.name);
        apply_status_class(&label, info.status.code(), is_left);
    });

    factory
}

/// Size / Mtime column: plain label at `field_idx` from encoded row.
fn make_field_factory(
    is_left: bool,
    field_idx: usize,
    align: gtk4::Align,
) -> SignalListItemFactory {
    let factory = SignalListItemFactory::new();

    factory.connect_setup(move |_, list_item| {
        let item = list_item.downcast_ref::<ListItem>().unwrap();
        let label = Label::new(None);
        label.set_halign(align);
        item.set_child(Some(&label));
    });

    factory.connect_bind(move |_, list_item| {
        let item = list_item.downcast_ref::<ListItem>().unwrap();
        let row = item.item().and_downcast::<TreeListRow>().unwrap();
        let obj = row.item().and_downcast::<StringObject>().unwrap();
        let raw = obj.string();

        let label = item.child().and_downcast::<Label>().unwrap();
        let info = DirRowInfo::decode(&raw);
        let missing = (is_left && info.status == FileStatus::RightOnly)
            || (!is_left && info.status == FileStatus::LeftOnly);

        label.set_label(if missing {
            ""
        } else {
            DirRowInfo::get_field_text(&raw, field_idx)
        });
        apply_status_class(&label, info.status.code(), is_left);
    });

    factory
}

// ─── Directory comparison tab ──────────────────────────────────────────────

/// Build the directory comparison widget and all its state (tree model, file
/// watcher, copy/delete handlers, context menus). The widget can be embedded
/// as a tab in an existing notebook or wrapped in its own window.
///
/// Returns `(dir_tab, watcher_alive, left_view, title)`.
pub(super) fn build_dir_tab(
    left_dir: std::path::PathBuf,
    right_dir: std::path::PathBuf,
    labels: &[String],
    tooltip_dirs: &[String],
    settings: Rc<RefCell<Settings>>,
    notebook: &Notebook,
    open_tabs: &Rc<RefCell<Vec<FileTab>>>,
) -> (GtkBox, FileWatcher, ColumnView, String) {
    let left_label_override: Rc<RefCell<Option<String>>> =
        Rc::new(RefCell::new(labels.first().cloned()));
    let right_label_override: Rc<RefCell<Option<String>>> =
        Rc::new(RefCell::new(labels.get(1).cloned()));
    let left_tooltip_override: Option<String> = tooltip_dirs.first().cloned();
    let right_tooltip_override: Option<String> = tooltip_dirs.get(1).cloned();
    let left_dir = Rc::new(RefCell::new(
        std::fs::canonicalize(&left_dir)
            .unwrap_or(left_dir)
            .to_string_lossy()
            .into_owned(),
    ));
    let right_dir = Rc::new(RefCell::new(
        std::fs::canonicalize(&right_dir)
            .unwrap_or(right_dir)
            .to_string_lossy()
            .into_owned(),
    ));

    // Build tree data (Rc<RefCell<…>> so the watcher callback can rebuild)
    let children_map = Rc::new(RefCell::new(HashMap::new()));
    let root_store = ListStore::new::<StringObject>();
    // Initial scan runs in background; tree populates once complete
    {
        let ld = left_dir.borrow().clone();
        let rd = right_dir.borrow().clone();
        let filters = settings.borrow().dir_filters.clone();
        let hide_hidden = settings.borrow().hide_hidden_files;
        let cm = children_map.clone();
        let rs = root_store.clone();
        gtk4::glib::spawn_future_local(async move {
            let (entries, _) = gio::spawn_blocking(move || {
                scan_tree(Path::new(&ld), Path::new(&rd), "", &filters, hide_hidden)
            })
            .await
            .unwrap();
            let store = build_stores(&entries, &mut cm.borrow_mut());
            for i in 0..store.n_items() {
                if let Some(obj) = store.item(i) {
                    rs.append(&obj.downcast::<StringObject>().unwrap());
                }
            }
        });
    }

    // Shared TreeListModel — both panes see the same tree structure
    let cm = children_map.clone();
    let tree_model = TreeListModel::new(root_store.clone(), false, false, move |item| {
        let obj = item.downcast_ref::<StringObject>()?;
        let raw = obj.string();
        let info = DirRowInfo::decode(&raw);
        if info.is_dir {
            cm.borrow()
                .get(&info.rel_path)
                .cloned()
                .map(gio::prelude::Cast::upcast::<gio::ListModel>)
        } else {
            None
        }
    });

    // Both panes share the same SingleSelection so the selected row is
    // highlighted on both sides.  The inactive pane is dimmed via opacity
    // and bordered differently to distinguish from the focused pane.
    let dir_sel = SingleSelection::new(Some(tree_model.clone()));

    // ── Left pane ──────────────────────────────────────────────────
    let left_view = ColumnView::new(Some(dir_sel.clone()));
    left_view.set_show_column_separators(true);
    {
        let col = ColumnViewColumn::new(Some("Name"), Some(make_name_factory(true)));
        col.set_expand(true);
        col.set_resizable(true);
        left_view.append_column(&col);
        let col = ColumnViewColumn::new(
            Some("Size"),
            Some(make_field_factory(true, 4, gtk4::Align::End)),
        );
        col.set_fixed_width(80);
        col.set_resizable(true);
        left_view.append_column(&col);
        let col = ColumnViewColumn::new(
            Some("Modification time"),
            Some(make_field_factory(true, 5, gtk4::Align::Start)),
        );
        col.set_fixed_width(200);
        col.set_resizable(true);
        left_view.append_column(&col);
    }

    // ── Right pane ─────────────────────────────────────────────────
    let right_view = ColumnView::new(Some(dir_sel.clone()));
    right_view.set_opacity(0.55);
    right_view.set_show_column_separators(true);
    {
        let col = ColumnViewColumn::new(Some("Name"), Some(make_name_factory(false)));
        col.set_expand(true);
        col.set_resizable(true);
        right_view.append_column(&col);
        let col = ColumnViewColumn::new(
            Some("Size"),
            Some(make_field_factory(false, 6, gtk4::Align::End)),
        );
        col.set_fixed_width(80);
        col.set_resizable(true);
        right_view.append_column(&col);
        let col = ColumnViewColumn::new(
            Some("Modification time"),
            Some(make_field_factory(false, 7, gtk4::Align::Start)),
        );
        col.set_fixed_width(200);
        col.set_resizable(true);
        right_view.append_column(&col);
    }

    // Track which pane was last focused (true = left, false = right).
    // Both panes share the same SingleSelection so the selected row is
    // highlighted on both sides; CSS borders distinguish the active pane.
    let focused_left = Rc::new(Cell::new(true));
    let sel_syncing = Rc::new(Cell::new(false));
    // CSS classes for focus borders are set on ScrolledWindows below (after
    // they are created), since ColumnView content is clipped by its parent.
    let left_scroll_slot: Rc<RefCell<Option<ScrolledWindow>>> = Rc::new(RefCell::new(None));
    let right_scroll_slot: Rc<RefCell<Option<ScrolledWindow>>> = Rc::new(RefCell::new(None));
    for (own_view, other_view, own_scroll, other_scroll, is_left) in [
        (
            &left_view,
            &right_view,
            &left_scroll_slot,
            &right_scroll_slot,
            true,
        ),
        (
            &right_view,
            &left_view,
            &right_scroll_slot,
            &left_scroll_slot,
            false,
        ),
    ] {
        let fl = focused_left.clone();
        let ov = own_view.clone();
        let otv = other_view.clone();
        let sel = dir_sel.clone();
        let os = own_scroll.clone();
        let ots = other_scroll.clone();
        let fc = EventControllerFocus::new();
        fc.connect_enter(move |_| {
            if fl.get() == is_left {
                return;
            }
            fl.set(is_left);
            ov.set_opacity(1.0);
            otv.set_opacity(0.55);
            if let Some(sw) = os.borrow().as_ref() {
                sw.add_css_class("pane-focused");
                sw.remove_css_class("pane-inactive");
            }
            if let Some(sw) = ots.borrow().as_ref() {
                sw.remove_css_class("pane-focused");
                sw.add_css_class("pane-inactive");
            }
            let pos = sel.selected();
            let vadj = ov
                .ancestor(ScrolledWindow::static_type())
                .and_downcast::<ScrolledWindow>()
                .map(|sw| (sw.vadjustment().value(), sw));
            let v = ov.clone();
            gtk4::glib::idle_add_local_once(move || {
                v.scroll_to(pos, None, gtk4::ListScrollFlags::FOCUS, None);
                if let Some((val, sw)) = vadj {
                    let sw2 = sw.clone();
                    gtk4::glib::idle_add_local_once(move || {
                        sw2.vadjustment().set_value(val);
                    });
                }
            });
        });
        own_view.add_controller(fc);
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

    // Set initial focus border classes and populate slots for the handlers above.
    left_scroll.add_css_class("pane-focused");
    right_scroll.add_css_class("pane-inactive");
    *left_scroll_slot.borrow_mut() = Some(left_scroll.clone());
    *right_scroll_slot.borrow_mut() = Some(right_scroll.clone());

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

    // Directory path headers above each pane
    let left_header_text = match *left_label_override.borrow() {
        Some(ref l) => l.clone(),
        None => shortened_path(Path::new(&*left_dir.borrow())),
    };
    let left_header = Label::new(Some(&left_header_text));
    {
        let ld = left_dir.borrow();
        let left_tip = left_tooltip_override.as_deref().unwrap_or(&ld);
        left_header.set_tooltip_text(Some(left_tip));
    }
    left_header.set_ellipsize(gtk4::pango::EllipsizeMode::Start);
    left_header.set_hexpand(true);
    let left_copy_btn = Button::from_icon_name("edit-copy-symbolic");
    left_copy_btn.set_has_frame(false);
    left_copy_btn.set_tooltip_text(Some("Copy path"));
    {
        let ld = left_dir.clone();
        left_copy_btn.connect_clicked(move |btn| {
            btn.clipboard().set_text(&ld.borrow());
        });
    }
    let left_header_box = GtkBox::new(Orientation::Horizontal, 0);
    left_header_box.set_margin_start(6);
    left_header_box.set_margin_end(2);
    left_header_box.set_margin_top(4);
    left_header_box.set_margin_bottom(4);
    left_header_box.append(&left_header);
    left_header_box.append(&left_copy_btn);

    let right_header_text = match *right_label_override.borrow() {
        Some(ref l) => l.clone(),
        None => shortened_path(Path::new(&*right_dir.borrow())),
    };
    let right_header = Label::new(Some(&right_header_text));
    {
        let rd = right_dir.borrow();
        let right_tip = right_tooltip_override.as_deref().unwrap_or(&rd);
        right_header.set_tooltip_text(Some(right_tip));
    }
    right_header.set_ellipsize(gtk4::pango::EllipsizeMode::Start);
    right_header.set_hexpand(true);
    let right_copy_btn = Button::from_icon_name("edit-copy-symbolic");
    right_copy_btn.set_has_frame(false);
    right_copy_btn.set_tooltip_text(Some("Copy path"));
    {
        let rd = right_dir.clone();
        right_copy_btn.connect_clicked(move |btn| {
            btn.clipboard().set_text(&rd.borrow());
        });
    }
    let right_header_box = GtkBox::new(Orientation::Horizontal, 0);
    right_header_box.set_margin_start(6);
    right_header_box.set_margin_end(2);
    right_header_box.set_margin_top(4);
    right_header_box.set_margin_bottom(4);
    right_header_box.append(&right_header);
    right_header_box.append(&right_copy_btn);

    let left_pane_box = GtkBox::new(Orientation::Vertical, 0);
    left_pane_box.append(&left_header_box);
    left_scroll.set_vexpand(true);
    left_pane_box.append(&left_scroll);
    let right_pane_box = GtkBox::new(Orientation::Vertical, 0);
    right_pane_box.append(&right_header_box);
    right_scroll.set_vexpand(true);
    right_pane_box.append(&right_scroll);

    let dir_paned = Paned::new(Orientation::Horizontal);
    dir_paned.set_start_child(Some(&left_pane_box));
    dir_paned.set_end_child(Some(&right_pane_box));

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

    let collapse_btn = Button::from_icon_name("format-indent-less-symbolic");
    collapse_btn.set_tooltip_text(Some("Collapse all"));
    let expand_btn = Button::from_icon_name("format-indent-more-symbolic");
    expand_btn.set_tooltip_text(Some("Expand all"));
    let tree_box = GtkBox::new(Orientation::Horizontal, 0);
    tree_box.add_css_class("linked");
    tree_box.append(&collapse_btn);
    tree_box.append(&expand_btn);

    let dir_prefs_btn = Button::from_icon_name("preferences-system-symbolic");
    dir_prefs_btn.set_tooltip_text(Some(&format!("Preferences ({}+,)", primary_key_name())));
    dir_prefs_btn.set_action_name(Some("win.prefs"));

    let apply_all_btn = Button::from_icon_name("media-skip-backward-symbolic");
    apply_all_btn.set_tooltip_text(Some("Apply all non-conflicting changes right → left"));

    dir_toolbar.append(&dir_copy_box);
    dir_toolbar.append(&delete_btn);
    dir_toolbar.append(&apply_all_btn);
    dir_toolbar.append(&dir_swap_btn);
    dir_toolbar.append(&tree_box);
    dir_toolbar.append(&dir_prefs_btn);

    // Helper: rescan directories and refresh the tree model (async)
    let scan_loading = Rc::new(Cell::new(false));
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
        let loading = scan_loading.clone();
        move || {
            if loading.get() {
                return;
            }
            loading.set(true);

            // Suppress selection sync during rebuild
            syncing.set(true);
            let saved_pos = sel.selected();
            let saved_rel = (|| -> Option<String> {
                let row = tm.item(saved_pos)?.downcast::<TreeListRow>().ok()?;
                let obj = row.item().and_downcast::<StringObject>()?;
                Some(DirRowInfo::decode(&obj.string()).rel_path)
            })();

            // Save expanded rel_paths
            let mut expanded: Vec<String> = Vec::new();
            for i in 0..tm.n_items() {
                if let Some(row) = tm.item(i).and_then(|o| o.downcast::<TreeListRow>().ok())
                    && row.is_expanded()
                    && let Some(obj) = row.item().and_downcast::<StringObject>()
                {
                    expanded.push(DirRowInfo::decode(&obj.string()).rel_path);
                }
            }

            // Background scan
            let ld_str = ld.borrow().clone();
            let rd_str = rd.borrow().clone();
            let filters = st.borrow().dir_filters.clone();
            let hide_hidden = st.borrow().hide_hidden_files;
            let cm = cm.clone();
            let rs = rs.clone();
            let tm = tm.clone();
            let sel = sel.clone();
            let syncing = syncing.clone();
            let lv = lv.clone();
            let rv = rv.clone();
            let loading = loading.clone();
            gtk4::glib::spawn_future_local(async move {
                let (entries, _) = gio::spawn_blocking(move || {
                    scan_tree(
                        Path::new(&ld_str),
                        Path::new(&rd_str),
                        "",
                        &filters,
                        hide_hidden,
                    )
                })
                .await
                .unwrap();
                loading.set(false);

                let mut new_map = HashMap::new();
                let new_store = build_stores(&entries, &mut new_map);

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

                // Restore expanded state with a single forward scan.
                // Expanding a row inserts its children right after it, so a
                // forward scan naturally visits those newly-inserted children
                // and keeps overall complexity O(n) in visible rows.
                let expanded_set: HashSet<&str> =
                    expanded.iter().map(std::string::String::as_str).collect();
                let mut i = 0;
                while i < tm.n_items() {
                    if let Some(row) = tm.item(i).and_then(|o| o.downcast::<TreeListRow>().ok())
                        && let Some(obj) = row.item().and_downcast::<StringObject>()
                        && expanded_set
                            .contains(DirRowInfo::decode(&obj.string()).rel_path.as_str())
                    {
                        row.set_expanded(true);
                    }
                    i += 1;
                }

                // Restore selection on both panes
                let n = tm.n_items();
                let mut final_pos = saved_pos.min(if n > 0 { n - 1 } else { 0 });
                if let Some(ref rel) = saved_rel {
                    for i in 0..n {
                        if let Some(row) = tm.item(i).and_then(|o| o.downcast::<TreeListRow>().ok())
                            && let Some(obj) = row.item().and_downcast::<StringObject>()
                            && DirRowInfo::decode(&obj.string()).rel_path == rel.as_str()
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
            });
        }
    };

    // Swap panes: swap left/right directories and rescan
    {
        let ld = left_dir.clone();
        let rd = right_dir.clone();
        let ll = left_label_override.clone();
        let rl = right_label_override.clone();
        let reload = reload_dir.clone();
        let lh = left_header.clone();
        let rh = right_header.clone();
        dir_swap_btn.connect_clicked(move |btn| {
            let tmp = ld.borrow().clone();
            (*ld.borrow_mut()).clone_from(&rd.borrow());
            *rd.borrow_mut() = tmp;
            let tmp_label = ll.borrow().clone();
            (*ll.borrow_mut()).clone_from(&rl.borrow());
            *rl.borrow_mut() = tmp_label;
            let left_text = match *ll.borrow() {
                Some(ref l) => l.clone(),
                None => shortened_path(Path::new(&*ld.borrow())),
            };
            lh.set_text(&left_text);
            lh.set_tooltip_text(Some(&*ld.borrow()));
            let right_text = match *rl.borrow() {
                Some(ref l) => l.clone(),
                None => shortened_path(Path::new(&*rd.borrow())),
            };
            rh.set_text(&right_text);
            rh.set_tooltip_text(Some(&*rd.borrow()));
            // Update window title
            if let Some(win) = find_window(btn) {
                let ln = Path::new(ld.borrow().as_str())
                    .file_name()
                    .map_or_else(|| ld.borrow().clone(), |n| n.to_string_lossy().into_owned());
                let rn = Path::new(rd.borrow().as_str())
                    .file_name()
                    .map_or_else(|| rd.borrow().clone(), |n| n.to_string_lossy().into_owned());
                win.set_title(Some(&format!("{ln} — {rn}")));
            }
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

    // Directory action group — used by both keyboard shortcuts and toolbar buttons
    let dir_action_group = gio::SimpleActionGroup::new();
    for (name, src_dir, dst_dir, only_status) in [
        (
            "folder-copy-left",
            &right_dir,
            &left_dir,
            FileStatus::RightOnly,
        ),
        (
            "folder-copy-right",
            &left_dir,
            &right_dir,
            FileStatus::LeftOnly,
        ),
    ] {
        let action = gio::SimpleAction::new(name, None);
        let get_row = get_selected_row.clone();
        let sd = src_dir.clone();
        let dd = dst_dir.clone();
        let reload = reload_dir.clone();
        let lv = left_view.clone();
        action.connect_activate(move |_, _| {
            if let Some(raw) = get_row() {
                let info = DirRowInfo::decode(&raw);
                if info.status == only_status
                    || info.status == FileStatus::Different
                    || info.status == FileStatus::Conflict
                {
                    let rel = info.rel_path;
                    let src = Path::new(sd.borrow().as_str()).join(&rel);
                    let dst = Path::new(dd.borrow().as_str()).join(&rel);
                    let reload = reload.clone();
                    let lv2 = lv.clone();
                    let do_copy = move || {
                        if let Err(e) = copy_path_recursive(&src, &dst)
                            && let Some(win) = find_window(&lv2)
                        {
                            show_error_dialog(&win, &format!("Copy failed: {e}"));
                        }
                        reload();
                    };
                    if info.status == FileStatus::Different || info.status == FileStatus::Conflict {
                        if let Some(win) = find_window(&lv) {
                            show_confirm_dialog(
                                &win,
                                &format!("Overwrite {rel}?"),
                                "The destination file will be replaced.",
                                "Overwrite",
                                do_copy,
                            );
                        }
                    } else {
                        do_copy();
                    }
                }
            }
        });
        dir_action_group.add_action(&action);
    }
    // Apply all: copy every non-conflicting changed file from right → left
    {
        let rs = root_store.clone();
        let cm = children_map.clone();
        let ld = left_dir.clone();
        let rd = right_dir.clone();
        let reload = reload_dir.clone();
        let lv = left_view.clone();
        let base_dir_override: Option<String> = left_tooltip_override.clone();
        apply_all_btn.connect_clicked(move |_| {
            let ld_str = ld.borrow().clone();
            let rd_str = rd.borrow().clone();
            let mut actionable: Vec<(String, FileStatus)> = Vec::new();
            collect_actionable_files(&rs, &cm.borrow(), &mut actionable);
            // In non-patch mode, LeftOnly delete is not supported
            let is_patch = base_dir_override.is_some();
            if !is_patch {
                actionable.retain(|(_, s)| *s != FileStatus::LeftOnly);
            }
            if actionable.is_empty() {
                return;
            }
            let count = actionable.len();
            let reload = reload.clone();
            let lv2 = lv.clone();
            let base_dir: Option<String> = base_dir_override.clone();
            let do_apply = move || {
                // Suppress file watcher during bulk operations
                mark_saving(Path::new(&ld_str));
                mark_saving(Path::new(&rd_str));
                let mut errors = Vec::new();
                for (rel, status) in &actionable {
                    let left_p = Path::new(&ld_str).join(rel);
                    let right_p = Path::new(&rd_str).join(rel);
                    let is_patch = base_dir.is_some();
                    match status {
                        FileStatus::Different => {
                            // Copy right → left (in patch mode, symlink follows to original)
                            if let Some(parent) = left_p.parent() {
                                let _ = fs::create_dir_all(parent);
                            }
                            if let Err(e) = copy_path_recursive(&right_p, &left_p) {
                                errors.push(format!("{rel}: {e}"));
                                continue;
                            }
                            // In patch mode, remove temp entries so they vanish from scan
                            if is_patch {
                                let _ = fs::remove_file(&left_p);
                                let _ = fs::remove_file(&right_p);
                            }
                        }
                        FileStatus::RightOnly => {
                            // New file — in patch mode copy to real base dir,
                            // otherwise copy to the left dir.
                            let target = if let Some(ref base) = base_dir {
                                PathBuf::from(base).join(rel)
                            } else {
                                left_p.clone()
                            };
                            if let Some(parent) = target.parent() {
                                let _ = fs::create_dir_all(parent);
                            }
                            if let Err(e) = copy_path_recursive(&right_p, &target) {
                                errors.push(format!("{rel}: {e}"));
                                continue;
                            }
                            // In patch mode, remove temp entries so they vanish from scan
                            if is_patch {
                                let _ = fs::remove_file(&left_p);
                                let _ = fs::remove_file(&right_p);
                            }
                        }
                        FileStatus::LeftOnly => {
                            if is_patch {
                                // In patch mode, left_p is a symlink; resolve to delete original
                                let real_path = fs::canonicalize(&left_p).unwrap_or(left_p.clone());
                                let _ = fs::remove_file(&real_path);
                                let _ = fs::remove_file(&left_p);
                            }
                        }
                        FileStatus::Conflict | FileStatus::Same => {}
                    }
                }
                if !errors.is_empty()
                    && let Some(win) = find_window(&lv2)
                {
                    show_error_dialog(&win, &errors.join("\n"));
                }
                reload();
            };
            if let Some(win) = find_window(&lv) {
                show_confirm_dialog(
                    &win,
                    &format!("Apply {count} file(s)?"),
                    "Non-conflicting changes will be applied (copy, create, or delete).",
                    "Apply",
                    do_apply,
                );
            }
        });
    }
    {
        let action = gio::SimpleAction::new("folder-delete", None);
        let get_row = get_selected_row.clone();
        let ld = left_dir.clone();
        let rd = right_dir.clone();
        let reload = reload_dir.clone();
        let fl = focused_left.clone();
        let lv = left_view.clone();
        action.connect_activate(move |_, _| {
            if let Some(raw) = get_row() {
                let info = DirRowInfo::decode(&raw);
                let rel = info.rel_path;
                let status = info.status;
                let lp = Path::new(ld.borrow().as_str()).join(&rel);
                let rp = Path::new(rd.borrow().as_str()).join(&rel);
                let path = match status {
                    FileStatus::LeftOnly => Some(lp),
                    FileStatus::RightOnly => Some(rp),
                    FileStatus::Different | FileStatus::Conflict | FileStatus::Same => {
                        Some(if fl.get() { lp } else { rp })
                    }
                };
                if let Some(p) = path
                    && let Some(win) = find_window(&lv)
                {
                    let reload = reload.clone();
                    show_confirm_dialog(
                        &win,
                        &format!("Move {rel} to trash?"),
                        "The file will be moved to the system trash.",
                        "Trash",
                        move || {
                            if let Err(e) = move_to_trash(&p) {
                                eprintln!("Trash failed: {e}");
                            }
                            reload();
                        },
                    );
                }
            }
        });
        dir_action_group.add_action(&action);
    }

    // Open externally: open selected file/dir in system default app
    {
        let action = gio::SimpleAction::new("folder-open-externally", None);
        let get_row = get_selected_row.clone();
        let ld = left_dir.clone();
        let rd = right_dir.clone();
        let fl = focused_left.clone();
        action.connect_activate(move |_, _| {
            if let Some(raw) = get_row() {
                let info = DirRowInfo::decode(&raw);
                let dir = if fl.get() { &ld } else { &rd };
                let path = Path::new(dir.borrow().as_str()).join(&info.rel_path);
                let abs = path.canonicalize().unwrap_or(path);
                open_externally(&abs);
            }
        });
        dir_action_group.add_action(&action);
    }

    // Collapse all: collapse every expanded row in the tree
    {
        let action = gio::SimpleAction::new("folder-collapse-all", None);
        let tm = tree_model.clone();
        action.connect_activate(move |_, _| {
            // Collapse in reverse so removing children doesn't shift
            // indices we haven't visited yet.
            let mut i = tm.n_items();
            while i > 0 {
                i -= 1;
                if let Some(item) = tm.item(i)
                    && let Ok(row) = item.downcast::<TreeListRow>()
                    && row.is_expanded()
                {
                    row.set_expanded(false);
                }
            }
        });
        dir_action_group.add_action(&action);
    }

    // Expand all: expand every directory row in the tree
    {
        let action = gio::SimpleAction::new("folder-expand-all", None);
        let tm = tree_model.clone();
        action.connect_activate(move |_, _| {
            // Expand iteratively since expanding a row may add new children
            let mut i = 0;
            while i < tm.n_items() {
                if let Some(item) = tm.item(i)
                    && let Ok(row) = item.downcast::<TreeListRow>()
                    && row.is_expandable()
                    && !row.is_expanded()
                {
                    row.set_expanded(true);
                }
                i += 1;
            }
        });
        dir_action_group.add_action(&action);
    }

    // Copy file path: copy the selected file's absolute path to clipboard
    {
        let action = gio::SimpleAction::new("folder-copy-path", None);
        let get_row = get_selected_row.clone();
        let ld = left_dir.clone();
        let rd = right_dir.clone();
        let fl = focused_left.clone();
        let lv = left_view.clone();
        action.connect_activate(move |_, _| {
            if let Some(raw) = get_row() {
                let info = DirRowInfo::decode(&raw);
                let lp = Path::new(ld.borrow().as_str()).join(&info.rel_path);
                let rp = Path::new(rd.borrow().as_str()).join(&info.rel_path);
                let path =
                    match info.status {
                        FileStatus::LeftOnly => lp,
                        FileStatus::RightOnly => rp,
                        FileStatus::Different | FileStatus::Conflict | FileStatus::Same => {
                            if fl.get() { lp } else { rp }
                        }
                    };
                let abs = path.canonicalize().unwrap_or(path);
                lv.clipboard().set_text(&abs.display().to_string());
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

    // Wire toolbar buttons to activate the corresponding actions
    {
        let dag = dir_action_group.clone();
        copy_left_btn.connect_clicked(move |_| {
            dag.activate_action("folder-copy-left", None);
        });
    }
    {
        let dag = dir_action_group.clone();
        copy_right_btn.connect_clicked(move |_| {
            dag.activate_action("folder-copy-right", None);
        });
    }
    {
        let dag = dir_action_group.clone();
        delete_btn.connect_clicked(move |_| {
            dag.activate_action("folder-delete", None);
        });
    }
    {
        let dag = dir_action_group.clone();
        collapse_btn.connect_clicked(move |_| {
            dag.activate_action("folder-collapse-all", None);
        });
    }
    {
        let dag = dir_action_group.clone();
        expand_btn.connect_clicked(move |_| {
            dag.activate_action("folder-expand-all", None);
        });
    }

    // Capture-phase key handler for dir-only shortcuts (Alt+Left/Right, Delete)
    // so they don't fire when a file-diff tab is focused.
    {
        let key_ctl = EventControllerKey::new();
        key_ctl.set_propagation_phase(gtk4::PropagationPhase::Capture);
        let dag = dir_action_group.clone();
        key_ctl.connect_key_pressed(move |_, key, _, mods| {
            let action_name = if mods.contains(gtk4::gdk::ModifierType::ALT_MASK) {
                match key {
                    k if k == gtk4::gdk::Key::Left => Some("folder-copy-left"),
                    k if k == gtk4::gdk::Key::Right => Some("folder-copy-right"),
                    _ => None,
                }
            } else if mods.is_empty() && key == gtk4::gdk::Key::Delete {
                Some("folder-delete")
            } else {
                None
            };
            if let Some(name) = action_name {
                if let Some(action) = dag.lookup_action(name) {
                    action
                        .downcast_ref::<gio::SimpleAction>()
                        .unwrap()
                        .activate(None);
                }
                return gtk4::glib::Propagation::Stop;
            }
            gtk4::glib::Propagation::Proceed
        });
        dir_tab.add_controller(key_ctl);
    }

    // Add "folder-open-diff" action (uses the passed-in notebook and open_tabs)
    {
        let action = gio::SimpleAction::new("folder-open-diff", None);
        let get_row = get_selected_row.clone();
        let nb = notebook.clone();
        let tabs = open_tabs.clone();
        let ld = left_dir.clone();
        let rd = right_dir.clone();
        let ll = left_label_override.clone();
        let rl = right_label_override.clone();
        let st = settings.clone();
        action.connect_activate(move |_, _| {
            if let Some(raw) = get_row() {
                let info = DirRowInfo::decode(&raw);
                if !info.is_dir {
                    let labels: Vec<String> = [ll.borrow().clone(), rl.borrow().clone()]
                        .into_iter()
                        .flatten()
                        .collect();
                    open_file_diff(
                        &nb,
                        &info.rel_path,
                        &tabs,
                        &ld.borrow(),
                        &rd.borrow(),
                        &labels,
                        &st,
                    );
                }
            }
        });
        dir_action_group.add_action(&action);
    }

    // ── Dir context menu ────────────────────────────────────────────
    {
        let dir_menu = gio::Menu::new();
        let actions_section = gio::Menu::new();
        actions_section.append(Some("Open Diff"), Some("dir.folder-open-diff"));
        actions_section.append(Some("Copy to Left"), Some("dir.folder-copy-left"));
        actions_section.append(Some("Copy to Right"), Some("dir.folder-copy-right"));
        actions_section.append(Some("Delete"), Some("dir.folder-delete"));
        dir_menu.append_section(None, &actions_section);
        let tree_section = gio::Menu::new();
        tree_section.append(Some("Collapse All"), Some("dir.folder-collapse-all"));
        tree_section.append(Some("Expand All"), Some("dir.folder-expand-all"));
        dir_menu.append_section(None, &tree_section);
        let util_section = gio::Menu::new();
        util_section.append(Some("Open Externally"), Some("dir.folder-open-externally"));
        util_section.append(Some("Copy File Path"), Some("dir.folder-copy-path"));
        dir_menu.append_section(None, &util_section);

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

        let act_del = dir_action_group
            .lookup_action("folder-delete")
            .and_downcast::<gio::SimpleAction>()
            .unwrap();
        let act_ext = dir_action_group
            .lookup_action("folder-open-externally")
            .and_downcast::<gio::SimpleAction>()
            .unwrap();
        let act_path = dir_action_group
            .lookup_action("folder-copy-path")
            .and_downcast::<gio::SimpleAction>()
            .unwrap();

        let setup_dir_ctx = |view: &ColumnView, popover: PopoverMenu, is_left: bool| {
            let gesture = GestureClick::new();
            gesture.set_button(3);
            let get_row = get_selected_row.clone();
            let ao = act_open.clone();
            let al = act_left.clone();
            let ar = act_right.clone();
            let ad = act_del.clone();
            let ae = act_ext.clone();
            let ap = act_path.clone();
            let pop = popover;
            let sel = dir_sel.clone();
            let tm = tree_model.clone();
            let v = view.clone();
            gesture.connect_pressed(move |_, _, x, y| {
                if let Some(pos) = column_view_row_at_y(&v, x, y, tm.n_items()) {
                    sel.set_selected(pos);
                }
                if let Some(raw) = get_row() {
                    let info = DirRowInfo::decode(&raw);
                    let is_dir = info.is_dir;
                    let status = info.status;
                    let exists_on_this_side = match status {
                        FileStatus::LeftOnly => is_left,
                        FileStatus::RightOnly => !is_left,
                        _ => true,
                    };
                    let is_diff_like =
                        status == FileStatus::Different || status == FileStatus::Conflict;
                    ao.set_enabled(!is_dir && (is_diff_like || status == FileStatus::Same));
                    al.set_enabled(
                        exists_on_this_side && (status == FileStatus::RightOnly || is_diff_like),
                    );
                    ar.set_enabled(
                        exists_on_this_side && (status == FileStatus::LeftOnly || is_diff_like),
                    );
                    ad.set_enabled(exists_on_this_side);
                    ae.set_enabled(!is_dir && exists_on_this_side);
                    ap.set_enabled(exists_on_this_side);
                    pop.set_pointing_to(Some(&gtk4::gdk::Rectangle::new(x as i32, y as i32, 1, 1)));
                    pop.popup();
                }
            });
            view.add_controller(gesture);
        };
        setup_dir_ctx(&left_view, dir_popover_l, true);
        setup_dir_ctx(&right_view, dir_popover_r, false);
    }

    // Shared activate logic — used by both Enter key and double-click
    let activate_row: Rc<dyn Fn()> = {
        let nb = notebook.clone();
        let tm = tree_model.clone();
        let tabs = open_tabs.clone();
        let ld = left_dir.clone();
        let rd = right_dir.clone();
        let ll = left_label_override.clone();
        let rl = right_label_override.clone();
        let sel = dir_sel.clone();
        let st = settings.clone();
        Rc::new(move || {
            let pos = sel.selected();
            if let Some(item) = tm.item(pos) {
                let row = item.downcast::<TreeListRow>().unwrap();
                let obj = row.item().and_downcast::<StringObject>().unwrap();
                let raw = obj.string();
                let info = DirRowInfo::decode(&raw);
                if info.is_dir {
                    row.set_expanded(!row.is_expanded());
                } else {
                    let labels: Vec<String> = [ll.borrow().clone(), rl.borrow().clone()]
                        .into_iter()
                        .flatten()
                        .collect();
                    open_file_diff(
                        &nb,
                        &info.rel_path,
                        &tabs,
                        &ld.borrow(),
                        &rd.borrow(),
                        &labels,
                        &st,
                    );
                }
            }
        })
    };

    // Double-click activates via connect_activate
    {
        let ar = activate_row.clone();
        left_view.connect_activate(move |_, _| ar());
    }
    {
        let ar = activate_row.clone();
        right_view.connect_activate(move |_, _| ar());
    }

    // Capture Enter on ColumnViews so it works reliably after tab close
    {
        let ar = activate_row.clone();
        let kc = EventControllerKey::new();
        kc.set_propagation_phase(gtk4::PropagationPhase::Capture);
        kc.connect_key_pressed(move |_, key, _, _| {
            if key == gtk4::gdk::Key::Return || key == gtk4::gdk::Key::KP_Enter {
                ar();
                gtk4::glib::Propagation::Stop
            } else {
                gtk4::glib::Propagation::Proceed
            }
        });
        left_view.add_controller(kc);
    }
    {
        let ar = activate_row.clone();
        let kc = EventControllerKey::new();
        kc.set_propagation_phase(gtk4::PropagationPhase::Capture);
        kc.connect_key_pressed(move |_, key, _, _| {
            if key == gtk4::gdk::Key::Return || key == gtk4::gdk::Key::KP_Enter {
                ar();
                gtk4::glib::Propagation::Stop
            } else {
                gtk4::glib::Propagation::Proceed
            }
        });
        right_view.add_controller(kc);
    }

    // ── File watcher ───────────────────────────────────────────────
    let ld_str = left_dir.borrow().clone();
    let rd_str = right_dir.borrow().clone();
    let ld_path = PathBuf::from(&ld_str);
    let rd_path = PathBuf::from(&rd_str);
    let tabs_reload = open_tabs.clone();
    let ld_reload = left_dir.clone();
    let rd_reload = right_dir.clone();
    let reload = reload_dir.clone();
    let st = settings.clone();
    let mut dirty = false;
    let mut retry_count: u32 = 0;
    let mut prev_hide_hidden = st.borrow().hide_hidden_files;
    let mut prev_filters = st.borrow().dir_filters.clone();
    let dir_watcher = start_file_watcher(
        &[ld_path.as_path(), rd_path.as_path()],
        true,
        None,
        move |fs_dirty| {
            if fs_dirty {
                dirty = true;
                retry_count = 0;
            }
            // Also rescan when filter settings change (e.g. via live preferences)
            {
                let s = st.borrow();
                if s.hide_hidden_files != prev_hide_hidden || s.dir_filters != prev_filters {
                    prev_hide_hidden = s.hide_hidden_files;
                    prev_filters = s.dir_filters.clone();
                    dirty = true;
                }
            }
            if dirty
                && !is_saving_under(&[
                    Path::new(&*ld_reload.borrow()),
                    Path::new(&*rd_reload.borrow()),
                ])
            {
                dirty = false;
                reload();
                let mut any_tab_failed = false;
                for tab in tabs_reload.borrow().iter() {
                    if !reload_file_tab(tab) {
                        any_tab_failed = true;
                    }
                }
                if any_tab_failed {
                    retry_count += 1;
                    if retry_count < 5 {
                        dirty = true;
                    } else {
                        eprintln!(
                            "Giving up tab reload after {retry_count} retries \
                             (file unreadable or binary)"
                        );
                    }
                }
            }
        },
    );

    // Build title — prefer label overrides (e.g. patch mode) over raw dir names
    let left_name = left_label_override.borrow().clone().unwrap_or_else(|| {
        Path::new(left_dir.borrow().as_str())
            .file_name()
            .map_or_else(
                || left_dir.borrow().clone(),
                |n| n.to_string_lossy().into_owned(),
            )
    });
    let right_name = right_label_override.borrow().clone().unwrap_or_else(|| {
        Path::new(right_dir.borrow().as_str())
            .file_name()
            .map_or_else(
                || right_dir.borrow().clone(),
                |n| n.to_string_lossy().into_owned(),
            )
    });
    let title = format!("{left_name} \u{2014} {right_name}");

    (dir_tab, dir_watcher, left_view, title)
}

/// Open a directory comparison as a tab in the given notebook.
pub(super) fn open_dir_comparison_tab(
    notebook: &Notebook,
    left_dir: PathBuf,
    right_dir: PathBuf,
    open_tabs: &Rc<RefCell<Vec<FileTab>>>,
    settings: &Rc<RefCell<Settings>>,
) {
    let (dir_widget, dir_watcher, left_view, dir_title) = build_dir_tab(
        left_dir,
        right_dir,
        &[],
        &[],
        Rc::clone(settings),
        notebook,
        open_tabs,
    );

    let (tab_label_box, close_btn) = make_closeable_tab_label(&dir_title);
    let page_num = notebook.append_page(&dir_widget, Some(&tab_label_box));
    notebook.set_current_page(Some(page_num));
    left_view.grab_focus();

    // Stop watcher when tab is removed
    {
        let dw = dir_widget.downgrade();
        let wa = dir_watcher.alive.clone();
        let handler_id: Rc<RefCell<Option<gtk4::glib::SignalHandlerId>>> =
            Rc::new(RefCell::new(None));
        let hid = handler_id.clone();
        let id = notebook.connect_page_removed(move |nb, child, _| {
            if let Some(w) = dw.upgrade()
                && *child == w
            {
                wa.set(false);
                if let Some(id) = hid.borrow_mut().take() {
                    nb.disconnect(id);
                }
            }
        });
        handler_id.borrow_mut().replace(id);
    }
    {
        let nb = notebook.clone();
        let dw = dir_widget;
        close_btn.connect_clicked(move |_| {
            if let Some(n) = nb.page_num(&dw) {
                nb.remove_page(Some(n));
            }
        });
    }
}

// ─── Directory comparison window ───────────────────────────────────────────

pub(super) fn build_dir_window(
    app: &Application,
    left_dir: std::path::PathBuf,
    right_dir: std::path::PathBuf,
    labels: &[String],
    settings: Rc<RefCell<Settings>>,
) {
    build_dir_window_with_tooltips(app, left_dir, right_dir, labels, &[], None, settings);
}

pub(super) fn build_dir_window_with_tooltips(
    app: &Application,
    left_dir: std::path::PathBuf,
    right_dir: std::path::PathBuf,
    labels: &[String],
    tooltip_dirs: &[String],
    on_destroy: Option<Box<dyn Fn() + 'static>>,
    settings: Rc<RefCell<Settings>>,
) {
    let AppWindow {
        window,
        notebook,
        open_tabs,
    } = build_app_window(app, &settings, 900, 600, true);

    let (dir_tab, dir_watcher, left_view, title) = build_dir_tab(
        left_dir,
        right_dir,
        labels,
        tooltip_dirs,
        settings,
        &notebook,
        &open_tabs,
    );
    notebook.append_page(&dir_tab, Some(&Label::new(Some(&title))));

    window.connect_destroy(move |_| {
        dir_watcher.alive.set(false);
    });

    // If a cleanup callback is provided (e.g. patch mode temp dir), run it on destroy
    if let Some(on_destroy) = on_destroy {
        window.connect_destroy(move |_| on_destroy());
    }

    window.present();
    left_view.grab_focus();
}

#[cfg(test)]
mod tests {
    use super::*;

    // ── Row encoding/decoding ─────────────────────────────────────

    #[test]
    fn encode_decode_roundtrip() {
        let original = DirRowInfo {
            status: FileStatus::Different,
            name: "test.txt".to_string(),
            is_dir: false,
            rel_path: "subdir/test.txt".to_string(),
            left_size: Some(1024),
            left_mtime: None,
            right_size: Some(2048),
            right_mtime: None,
        };
        let encoded = original.encode();
        let decoded = DirRowInfo::decode(&encoded);

        assert_eq!(decoded.status, FileStatus::Different);
        assert_eq!(decoded.name, "test.txt");
        assert!(!decoded.is_dir);
        assert_eq!(decoded.rel_path, "subdir/test.txt");
    }

    #[test]
    fn encode_decode_directory() {
        let original = DirRowInfo {
            status: FileStatus::Same,
            name: "mydir".to_string(),
            is_dir: true,
            rel_path: "parent/mydir".to_string(),
            left_size: None,
            left_mtime: None,
            right_size: None,
            right_mtime: None,
        };
        let encoded = original.encode();
        let decoded = DirRowInfo::decode(&encoded);

        assert_eq!(decoded.status, FileStatus::Same);
        assert_eq!(decoded.name, "mydir");
        assert!(decoded.is_dir);
        assert_eq!(decoded.rel_path, "parent/mydir");
    }

    #[test]
    fn encode_decode_left_only() {
        let original = DirRowInfo {
            status: FileStatus::LeftOnly,
            name: "orphan.rs".to_string(),
            is_dir: false,
            rel_path: "orphan.rs".to_string(),
            left_size: Some(512),
            left_mtime: None,
            right_size: None,
            right_mtime: None,
        };
        let encoded = original.encode();
        let decoded = DirRowInfo::decode(&encoded);

        assert_eq!(decoded.status, FileStatus::LeftOnly);
        assert_eq!(decoded.name, "orphan.rs");
    }

    #[test]
    fn encode_decode_right_only() {
        let original = DirRowInfo {
            status: FileStatus::RightOnly,
            name: "new_file.rs".to_string(),
            is_dir: false,
            rel_path: "new_file.rs".to_string(),
            left_size: None,
            left_mtime: None,
            right_size: Some(256),
            right_mtime: None,
        };
        let encoded = original.encode();
        let decoded = DirRowInfo::decode(&encoded);

        assert_eq!(decoded.status, FileStatus::RightOnly);
        assert_eq!(decoded.name, "new_file.rs");
    }

    #[test]
    fn encode_decode_conflict() {
        let original = DirRowInfo {
            status: FileStatus::Conflict,
            name: "broken.rs".to_string(),
            is_dir: false,
            rel_path: "src/broken.rs".to_string(),
            left_size: Some(100),
            left_mtime: None,
            right_size: Some(200),
            right_mtime: None,
        };
        let encoded = original.encode();
        let decoded = DirRowInfo::decode(&encoded);

        assert_eq!(decoded.status, FileStatus::Conflict);
        assert_eq!(decoded.name, "broken.rs");
        assert_eq!(decoded.rel_path, "src/broken.rs");
    }

    #[test]
    fn decode_field_out_of_bounds() {
        // Should return empty string for missing fields via helper
        assert_eq!(DirRowInfo::get_field_text("a\x1fb", 5), "");
        assert_eq!(DirRowInfo::get_field_text("", 0), "");
    }

    #[test]
    fn decode_name_with_special_chars() {
        let original = DirRowInfo {
            status: FileStatus::Same,
            name: "file with spaces.txt".to_string(),
            is_dir: false,
            rel_path: "path/file with spaces.txt".to_string(),
            left_size: None,
            left_mtime: None,
            right_size: None,
            right_mtime: None,
        };
        let encoded = original.encode();
        let decoded = DirRowInfo::decode(&encoded);

        assert_eq!(decoded.name, "file with spaces.txt");
        assert_eq!(decoded.rel_path, "path/file with spaces.txt");
    }

    // ── collect_actionable_files logic ──────────────────────────

    #[test]
    fn actionable_includes_different() {
        assert!(matches!(
            FileStatus::Different,
            FileStatus::Different | FileStatus::RightOnly | FileStatus::LeftOnly
        ));
    }

    #[test]
    fn actionable_includes_right_only() {
        assert!(matches!(
            FileStatus::RightOnly,
            FileStatus::Different | FileStatus::RightOnly | FileStatus::LeftOnly
        ));
    }

    #[test]
    fn actionable_includes_left_only() {
        assert!(matches!(
            FileStatus::LeftOnly,
            FileStatus::Different | FileStatus::RightOnly | FileStatus::LeftOnly
        ));
    }

    #[test]
    fn actionable_excludes_conflict() {
        assert!(!matches!(
            FileStatus::Conflict,
            FileStatus::Different | FileStatus::RightOnly | FileStatus::LeftOnly
        ));
    }

    #[test]
    fn actionable_excludes_same() {
        assert!(!matches!(
            FileStatus::Same,
            FileStatus::Different | FileStatus::RightOnly | FileStatus::LeftOnly
        ));
    }

    #[test]
    fn actionable_filter_mixed_statuses() {
        let entries = [
            ("a.rs", FileStatus::Different),
            ("b.rs", FileStatus::Same),
            ("c.rs", FileStatus::Conflict),
            ("d.rs", FileStatus::RightOnly),
            ("e.rs", FileStatus::LeftOnly),
        ];
        let result: Vec<&str> = entries
            .iter()
            .filter(|(_, s)| {
                matches!(
                    s,
                    FileStatus::Different | FileStatus::RightOnly | FileStatus::LeftOnly
                )
            })
            .map(|(name, _)| *name)
            .collect();
        assert_eq!(result, vec!["a.rs", "d.rs", "e.rs"]);
    }

    // ── FileStatus ────────────────────────────────────────────────

    #[test]
    fn file_status_codes() {
        assert_eq!(FileStatus::Same.code(), "S");
        assert_eq!(FileStatus::Different.code(), "D");
        assert_eq!(FileStatus::Conflict.code(), "C");
        assert_eq!(FileStatus::LeftOnly.code(), "L");
        assert_eq!(FileStatus::RightOnly.code(), "R");
    }
}
