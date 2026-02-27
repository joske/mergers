#[allow(clippy::wildcard_imports)]
use super::*;

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
    // Validate rel_path doesn't escape temp_dir via '..' components
    for component in Path::new(rel_path).components() {
        if matches!(component, std::path::Component::ParentDir) {
            eprintln!("Refusing to open path with '..' component: {rel_path}");
            return;
        }
    }

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
            if !temp_file.starts_with(temp_dir) {
                eprintln!("Path escapes temp directory: {rel_path}");
                return;
            }
            if let Some(parent) = temp_file.parent() {
                let _ = fs::create_dir_all(parent);
            }
            let _ = fs::write(&temp_file, &head);
            (temp_file, working_path)
        }
        "A" | "U" => {
            let temp_file = temp_dir.join(format!("__empty__{rel_path}"));
            if !temp_file.starts_with(temp_dir) {
                eprintln!("Path escapes temp directory: {rel_path}");
                return;
            }
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
            if !temp_left.starts_with(temp_dir) || !temp_right.starts_with(temp_dir) {
                eprintln!("Path escapes temp directory: {rel_path}");
                return;
            }
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
        widget: dv.widget.clone(),
        left_path: Rc::new(RefCell::new(left_path.display().to_string())),
        right_path: Rc::new(RefCell::new(right_path.display().to_string())),
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

    {
        let nb = notebook.clone();
        let w = dv.widget.clone();
        let tabs = open_tabs.clone();
        close_btn.connect_clicked(move |_| {
            if let Some(n) = nb.page_num(&w) {
                let win = nb
                    .root()
                    .and_then(|r| r.downcast::<ApplicationWindow>().ok());
                if let Some(win) = win {
                    close_notebook_tab(&win, &nb, &tabs, n);
                } else {
                    nb.remove_page(Some(n));
                    tabs.borrow_mut().retain(|t| t.id != tab_id);
                }
            }
        });
    }
}

pub(super) fn build_vcs_window(
    app: &Application,
    dir: std::path::PathBuf,
    settings: Rc<RefCell<Settings>>,
) {
    let Some(repo_root) = crate::vcs::repo_root(&dir) else {
        eprintln!("Error: could not determine git repository root");
        return;
    };

    let temp_dir = std::env::temp_dir().join(format!("mergers-{}", std::process::id()));
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
    let vcs_watcher_alive = Rc::new(Cell::new(true));
    let (fs_tx, fs_rx) = mpsc::channel::<()>();
    let vcs_watcher = {
        use notify::{RecursiveMode, Watcher};
        let mut w =
            notify::recommended_watcher(move |res: Result<notify::Event, notify::Error>| {
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
            })
            .expect("Failed to create file watcher");
        w.watch(&repo_root, RecursiveMode::Recursive).ok();
        w
    };
    {
        let r = refresh.clone();
        let alive = vcs_watcher_alive.clone();
        let mut dirty = false;
        gtk4::glib::timeout_add_local(Duration::from_millis(500), move || {
            let _ = &vcs_watcher; // prevent drop; watcher lives until closure is dropped
            if !alive.get() {
                return gtk4::glib::ControlFlow::Break;
            }
            while fs_rx.try_recv().is_ok() {
                dirty = true;
            }
            if dirty {
                dirty = false;
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
        let v = view.clone();
        action.connect_activate(move |_, _| {
            if let Some(item) = s.selected_item() {
                let obj = item.downcast::<StringObject>().unwrap();
                let raw = obj.string();
                let rel = decode_vcs_path(&raw).to_string();
                if let Some(win) = v
                    .root()
                    .and_then(|root| root.downcast::<ApplicationWindow>().ok())
                {
                    let rr = rr.clone();
                    let r = r.clone();
                    show_confirm_dialog(
                        &win,
                        &format!("Discard changes to {rel}?"),
                        "This cannot be undone.",
                        "Discard",
                        move || {
                            crate::vcs::discard_changes(&rr, &rel);
                            r();
                        },
                    );
                }
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
        let v = view.clone();
        action.connect_activate(move |_, _| {
            if let Some(item) = s.selected_item() {
                let obj = item.downcast::<StringObject>().unwrap();
                let raw = obj.string();
                let rel = decode_vcs_path(&raw).to_string();
                if let Some(win) = v
                    .root()
                    .and_then(|root| root.downcast::<ApplicationWindow>().ok())
                {
                    let rr = rr.clone();
                    let r = r.clone();
                    show_confirm_dialog(
                        &win,
                        &format!("Move {rel} to trash?"),
                        "The file will be moved to the system trash.",
                        "Trash",
                        move || {
                            let path = rr.join(&rel);
                            if let Err(e) = gio::File::for_path(&path).trash(gio::Cancellable::NONE)
                            {
                                eprintln!("Trash failed: {e}");
                            }
                            r();
                        },
                    );
                }
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
        let v = view.clone();
        let st = store.clone();
        gesture.connect_pressed(move |_, _, x, y| {
            if let Some(pos) = column_view_row_at_y(&v, x, y, st.n_items()) {
                s.set_selected(pos);
            }
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
        .title(format!("mergers — {repo_name} (git)"))
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

    // Unsaved-changes guard on window close button
    {
        let tabs = open_tabs.clone();
        window.connect_close_request(move |w| handle_notebook_close_request(w, &tabs));
    }

    if let Some(gtk_app) = window.application() {
        gtk_app.set_accels_for_action("win.prefs", &["<Ctrl>comma"]);
        gtk_app.set_accels_for_action("win.close-tab", &["<Ctrl>w"]);
    }

    // Clean up temp dir and stop watcher on destroy
    let td = temp_dir.clone();
    window.connect_destroy(move |_| {
        vcs_watcher_alive.set(false);
        let _ = fs::remove_dir_all(&td);
    });

    window.present();
    view.grab_focus();
}

#[cfg(test)]
mod tests {
    use super::*;

    // ── vcs_status_label ─────────────────────────────────────────

    #[test]
    fn vcs_status_labels() {
        assert_eq!(vcs_status_label("M"), "Modified");
        assert_eq!(vcs_status_label("A"), "Added");
        assert_eq!(vcs_status_label("D"), "Deleted");
        assert_eq!(vcs_status_label("R"), "Renamed");
        assert_eq!(vcs_status_label("U"), "Untracked");
        assert_eq!(vcs_status_label("X"), "");
    }

    // ── vcs_status_css ───────────────────────────────────────────

    #[test]
    fn vcs_status_css_classes() {
        assert_eq!(vcs_status_css("M"), "diff-changed");
        assert_eq!(vcs_status_css("R"), "diff-changed");
        assert_eq!(vcs_status_css("A"), "diff-inserted");
        assert_eq!(vcs_status_css("U"), "diff-inserted");
        assert_eq!(vcs_status_css("D"), "diff-deleted");
        assert_eq!(vcs_status_css("?"), "");
    }
}
