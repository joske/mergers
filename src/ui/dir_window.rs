#[allow(clippy::wildcard_imports)]
use super::*;

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

// ─── Directory comparison window ───────────────────────────────────────────

pub(super) fn build_dir_window(
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

    // Copy to left: right → left (with confirmation when overwriting)
    {
        let get_row = get_selected_row.clone();
        let ld = left_dir.clone();
        let rd = right_dir.clone();
        let reload = reload_dir.clone();
        let lv = left_view.clone();
        copy_left_btn.connect_clicked(move |_| {
            if let Some(raw) = get_row() {
                let rel = decode_rel_path(&raw).to_string();
                let status = decode_status(&raw);
                if status == "R" || status == "D" {
                    let src = Path::new(rd.borrow().as_str()).join(&rel);
                    let dst = Path::new(ld.borrow().as_str()).join(&rel);
                    let reload = reload.clone();
                    let lv2 = lv.clone();
                    let do_copy = move || {
                        if let Err(e) = copy_path_recursive(&src, &dst)
                            && let Some(win) = lv2
                                .root()
                                .and_then(|r| r.downcast::<ApplicationWindow>().ok())
                        {
                            show_error_dialog(&win, &format!("Copy failed: {e}"));
                        }
                        reload();
                    };
                    if status == "D" {
                        if let Some(win) = lv
                            .root()
                            .and_then(|r| r.downcast::<ApplicationWindow>().ok())
                        {
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
    }

    // Copy to right: left → right (with confirmation when overwriting)
    {
        let get_row = get_selected_row.clone();
        let ld = left_dir.clone();
        let rd = right_dir.clone();
        let reload = reload_dir.clone();
        let lv = left_view.clone();
        copy_right_btn.connect_clicked(move |_| {
            if let Some(raw) = get_row() {
                let rel = decode_rel_path(&raw).to_string();
                let status = decode_status(&raw);
                if status == "L" || status == "D" {
                    let src = Path::new(ld.borrow().as_str()).join(&rel);
                    let dst = Path::new(rd.borrow().as_str()).join(&rel);
                    let reload = reload.clone();
                    let lv2 = lv.clone();
                    let do_copy = move || {
                        if let Err(e) = copy_path_recursive(&src, &dst)
                            && let Some(win) = lv2
                                .root()
                                .and_then(|r| r.downcast::<ApplicationWindow>().ok())
                        {
                            show_error_dialog(&win, &format!("Copy failed: {e}"));
                        }
                        reload();
                    };
                    if status == "D" {
                        if let Some(win) = lv
                            .root()
                            .and_then(|r| r.downcast::<ApplicationWindow>().ok())
                        {
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
    }

    // Delete selected (trash with confirmation)
    {
        let get_row = get_selected_row.clone();
        let ld = left_dir.clone();
        let rd = right_dir.clone();
        let reload = reload_dir.clone();
        let fl = focused_left.clone();
        let lv = left_view.clone();
        delete_btn.connect_clicked(move |_| {
            if let Some(raw) = get_row() {
                let rel = decode_rel_path(&raw).to_string();
                let status = decode_status(&raw).to_string();
                let lp = Path::new(ld.borrow().as_str()).join(&rel);
                let rp = Path::new(rd.borrow().as_str()).join(&rel);
                let path = match status.as_str() {
                    "L" => Some(lp),
                    "R" => Some(rp),
                    "D" | "S" => Some(if fl.get() { lp } else { rp }),
                    _ => None,
                };
                if let Some(p) = path
                    && let Some(win) = lv
                        .root()
                        .and_then(|r| r.downcast::<ApplicationWindow>().ok())
                {
                    let reload = reload.clone();
                    show_confirm_dialog(
                        &win,
                        &format!("Move {rel} to trash?"),
                        "The file will be moved to the system trash.",
                        "Trash",
                        move || {
                            if let Err(e) = gio::File::for_path(&p).trash(gio::Cancellable::NONE) {
                                eprintln!("Trash failed: {e}");
                            }
                            reload();
                        },
                    );
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
        let lv = left_view.clone();
        action.connect_activate(move |_, _| {
            if let Some(raw) = get_row() {
                let rel = decode_rel_path(&raw).to_string();
                let status = decode_status(&raw);
                if status == "R" || status == "D" {
                    let src = Path::new(rd.borrow().as_str()).join(&rel);
                    let dst = Path::new(ld.borrow().as_str()).join(&rel);
                    let reload = reload.clone();
                    let lv2 = lv.clone();
                    let do_copy = move || {
                        if let Err(e) = copy_path_recursive(&src, &dst)
                            && let Some(win) = lv2
                                .root()
                                .and_then(|r| r.downcast::<ApplicationWindow>().ok())
                        {
                            show_error_dialog(&win, &format!("Copy failed: {e}"));
                        }
                        reload();
                    };
                    if status == "D" {
                        if let Some(win) = lv
                            .root()
                            .and_then(|r| r.downcast::<ApplicationWindow>().ok())
                        {
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
    {
        let action = gio::SimpleAction::new("folder-copy-right", None);
        let get_row = get_selected_row.clone();
        let ld = left_dir.clone();
        let rd = right_dir.clone();
        let reload = reload_dir.clone();
        let lv = left_view.clone();
        action.connect_activate(move |_, _| {
            if let Some(raw) = get_row() {
                let rel = decode_rel_path(&raw).to_string();
                let status = decode_status(&raw);
                if status == "L" || status == "D" {
                    let src = Path::new(ld.borrow().as_str()).join(&rel);
                    let dst = Path::new(rd.borrow().as_str()).join(&rel);
                    let reload = reload.clone();
                    let lv2 = lv.clone();
                    let do_copy = move || {
                        if let Err(e) = copy_path_recursive(&src, &dst)
                            && let Some(win) = lv2
                                .root()
                                .and_then(|r| r.downcast::<ApplicationWindow>().ok())
                        {
                            show_error_dialog(&win, &format!("Copy failed: {e}"));
                        }
                        reload();
                    };
                    if status == "D" {
                        if let Some(win) = lv
                            .root()
                            .and_then(|r| r.downcast::<ApplicationWindow>().ok())
                        {
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
                let rel = decode_rel_path(&raw).to_string();
                let status = decode_status(&raw).to_string();
                let lp = Path::new(ld.borrow().as_str()).join(&rel);
                let rp = Path::new(rd.borrow().as_str()).join(&rel);
                let path = match status.as_str() {
                    "L" => Some(lp),
                    "R" => Some(rp),
                    "D" | "S" => Some(if fl.get() { lp } else { rp }),
                    _ => None,
                };
                if let Some(p) = path
                    && let Some(win) = lv
                        .root()
                        .and_then(|r| r.downcast::<ApplicationWindow>().ok())
                {
                    let reload = reload.clone();
                    show_confirm_dialog(
                        &win,
                        &format!("Move {rel} to trash?"),
                        "The file will be moved to the system trash.",
                        "Trash",
                        move || {
                            if let Err(e) = gio::File::for_path(&p).trash(gio::Cancellable::NONE) {
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
    let dir_watcher_alive = Rc::new(Cell::new(true));
    let (fs_tx, fs_rx) = mpsc::channel::<()>();
    let dir_watcher = {
        use notify::{RecursiveMode, Watcher};
        let ld = left_dir.borrow().clone();
        let rd = right_dir.borrow().clone();
        let mut w =
            notify::recommended_watcher(move |res: Result<notify::Event, notify::Error>| {
                if res.is_ok() {
                    let _ = fs_tx.send(());
                }
            })
            .expect("Failed to create file watcher");
        w.watch(Path::new(&ld), RecursiveMode::Recursive).ok();
        w.watch(Path::new(&rd), RecursiveMode::Recursive).ok();
        w
    };

    // Poll for filesystem changes and reload
    let tabs_reload = open_tabs.clone();
    let ld_reload = left_dir.clone();
    let rd_reload = right_dir.clone();
    {
        let reload = reload_dir.clone();
        let alive = dir_watcher_alive.clone();
        let mut dirty = false;
        gtk4::glib::timeout_add_local(Duration::from_millis(500), move || {
            let _ = &dir_watcher; // prevent drop; watcher lives until closure is dropped
            if !alive.get() {
                return gtk4::glib::ControlFlow::Break;
            }
            while fs_rx.try_recv().is_ok() {
                dirty = true;
            }
            if dirty
                && !is_saving_under(&[
                    Path::new(&*ld_reload.borrow()),
                    Path::new(&*rd_reload.borrow()),
                ])
            {
                dirty = false;
                reload();
                // Reload open file tabs; keep dirty if any tab read fails
                for tab in tabs_reload.borrow().iter() {
                    if !reload_file_tab(tab, &ld_reload.borrow(), &rd_reload.borrow()) {
                        dirty = true;
                    }
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
    // Close-tab action (Ctrl+W) — close current file tab or window
    {
        let action = gio::SimpleAction::new("close-tab", None);
        let nb = notebook.clone();
        let w = window.clone();
        let tabs = open_tabs.clone();
        action.connect_activate(move |_, _| match nb.current_page() {
            Some(0) | None => w.close(),
            Some(n) => close_notebook_tab(&w, &nb, &tabs, n),
        });
        win_actions.add_action(&action);
    }
    window.insert_action_group("win", Some(&win_actions));
    window.insert_action_group("dir", Some(&dir_action_group));

    // Unsaved-changes guard on window close button
    {
        let tabs = open_tabs.clone();
        window.connect_close_request(move |w| handle_notebook_close_request(w, &tabs));
    }

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
        gtk_app.set_accels_for_action("win.close-tab", &["<Ctrl>w"]);
        // Directory copy actions
        gtk_app.set_accels_for_action("dir.folder-copy-left", &["<Alt>Left"]);
        gtk_app.set_accels_for_action("dir.folder-copy-right", &["<Alt>Right"]);
        gtk_app.set_accels_for_action("dir.folder-delete", &["Delete"]);
    }

    window.connect_destroy(move |_| {
        dir_watcher_alive.set(false);
    });

    window.present();
    left_view.grab_focus();
}
