#[allow(clippy::wildcard_imports)]
use super::*;

// ─── VCS Logic (Pure Functions & Helpers) ───────────────────────────────────

pub fn encode_vcs_row(status: &crate::vcs::VcsStatus, rel_path: &str, extra: &str) -> String {
    let code = match status {
        crate::vcs::VcsStatus::Modified => "M",
        crate::vcs::VcsStatus::Added => "A",
        crate::vcs::VcsStatus::Deleted => "D",
        crate::vcs::VcsStatus::Renamed => "R",
        crate::vcs::VcsStatus::Untracked => "U",
        crate::vcs::VcsStatus::Conflict => "C",
    };
    format!("{code}{SEP}{rel_path}{SEP}{extra}")
}

/// Returns `(status_code, rel_path, extra)`.
pub fn decode_vcs_row(raw: &str) -> (&str, &str, &str) {
    let mut parts = raw.splitn(3, SEP);
    let code = parts.next().unwrap_or("");
    let path = parts.next().unwrap_or("");
    let extra = parts.next().unwrap_or("");
    (code, path, extra)
}

pub fn vcs_status_info(code: &str) -> (&'static str, &'static str) {
    match code {
        "M" => ("Modified", "diff-changed"),
        "A" => ("Added", "diff-inserted"),
        "D" => ("Deleted", "diff-deleted"),
        "R" => ("Renamed", "diff-changed"),
        "U" => ("Untracked", "diff-inserted"),
        "C" => ("Conflict", "diff-conflict"),
        _ => ("", ""),
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum VcsDiffPlan {
    /// Show diff between a HEAD version (saved to temp) and the working file.
    WithWorking {
        temp_head: PathBuf,
        working: PathBuf,
        content: String,
    },
    /// Show diff between an empty temp file and the working file.
    EmptyWithWorking {
        temp_empty: PathBuf,
        working: PathBuf,
    },
    /// Show diff between a HEAD version (saved to temp) and a "deleted" temp file.
    WithDeleted {
        temp_head: PathBuf,
        temp_deleted: PathBuf,
        content: String,
    },
    /// 3-way merge for conflict: ours (left), working copy (middle), theirs (right).
    ConflictMerge {
        temp_ours: PathBuf,
        ours_content: String,
        working: PathBuf,
        temp_theirs: PathBuf,
        theirs_content: String,
    },
}

pub fn plan_vcs_diff(
    repo_root: &Path,
    temp_dir: &Path,
    rel_path: &str,
    status_code: &str,
) -> Option<VcsDiffPlan> {
    // Security: Validate rel_path doesn't escape via '..' components
    if Path::new(rel_path)
        .components()
        .any(|c| matches!(c, std::path::Component::ParentDir))
    {
        eprintln!("Refusing to open path with '..' component: {rel_path}");
        return None;
    }

    let working_path = repo_root.join(rel_path);

    match status_code {
        "C" => {
            let ours = crate::vcs::stage_content(repo_root, rel_path, 2).unwrap_or_default();
            let theirs = crate::vcs::stage_content(repo_root, rel_path, 3).unwrap_or_default();
            Some(VcsDiffPlan::ConflictMerge {
                temp_ours: temp_dir.join(format!("__ours__{rel_path}")),
                ours_content: ours,
                working: working_path,
                temp_theirs: temp_dir.join(format!("__theirs__{rel_path}")),
                theirs_content: theirs,
            })
        }
        "M" | "R" => {
            let content = crate::vcs::head_content(repo_root, rel_path).unwrap_or_default();
            Some(VcsDiffPlan::WithWorking {
                temp_head: temp_dir.join(rel_path),
                working: working_path,
                content,
            })
        }
        "A" | "U" => Some(VcsDiffPlan::EmptyWithWorking {
            temp_empty: temp_dir.join(format!("__empty__{rel_path}")),
            working: working_path,
        }),
        "D" => {
            let content = crate::vcs::head_content(repo_root, rel_path).unwrap_or_default();
            Some(VcsDiffPlan::WithDeleted {
                temp_head: temp_dir.join(rel_path),
                temp_deleted: temp_dir.join(format!("__deleted__{rel_path}")),
                content,
            })
        }
        _ => None,
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
        let (code, _, _) = decode_vcs_row(&raw);
        let (text, css) = vcs_status_info(code);

        label.set_label(text);
        for cls in &[
            "diff-changed",
            "diff-deleted",
            "diff-inserted",
            "diff-conflict",
        ] {
            label.remove_css_class(cls);
        }
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

        let (code, path, _) = decode_vcs_row(&raw);
        let (_, css) = vcs_status_info(code);

        icon.set_icon_name(Some("text-x-generic-symbolic"));
        label.set_label(path);
        for cls in &[
            "diff-changed",
            "diff-deleted",
            "diff-inserted",
            "diff-conflict",
        ] {
            label.remove_css_class(cls);
        }
        if !css.is_empty() {
            label.add_css_class(css);
        }
    });
    factory
}

fn make_vcs_extra_factory() -> SignalListItemFactory {
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
        let (_, _, extra) = decode_vcs_row(&raw);
        label.set_label(extra);
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
    if let Some(page) = open_tabs
        .borrow()
        .iter()
        .find(|t| t.rel_path() == rel_path)
        .and_then(|t| notebook.page_num(t.widget()))
    {
        notebook.set_current_page(Some(page));
        return;
    }

    let Some(plan) = plan_vcs_diff(repo_root, temp_dir, rel_path, status_code) else {
        return;
    };

    let (status_text, _) = vcs_status_info(status_code);
    let file_name = Path::new(rel_path).file_name().map_or_else(
        || rel_path.to_string(),
        |n| n.to_string_lossy().into_owned(),
    );
    let tab_title = format!("[{status_text}] {file_name}");

    // Build tab label with close button
    let tab_label_box = GtkBox::new(Orientation::Horizontal, 4);
    let label = Label::new(Some(&tab_title));
    let close_btn = Button::from_icon_name("window-close-symbolic");
    close_btn.set_has_frame(false);
    tab_label_box.append(&label);
    tab_label_box.append(&close_btn);

    let tab_id = NEXT_TAB_ID.fetch_add(1, std::sync::atomic::Ordering::Relaxed);

    if let VcsDiffPlan::ConflictMerge {
        temp_ours,
        ours_content,
        working,
        temp_theirs,
        theirs_content,
    } = plan
    {
        // Write temp files for ours/theirs
        for (p, content) in [(&temp_ours, &ours_content), (&temp_theirs, &theirs_content)] {
            if let Some(parent) = p.parent() {
                let _ = fs::create_dir_all(parent);
            }
            let _ = fs::write(p, content);
        }

        let labels = vec![
            format!("Ours: {rel_path}"),
            format!("Working: {rel_path}"),
            format!("Theirs: {rel_path}"),
        ];
        let mv = build_merge_view(&temp_ours, &working, &temp_theirs, &labels, settings);
        mv.widget
            .insert_action_group("diff", Some(&mv.action_group));

        open_tabs.borrow_mut().push(FileTab::Merge {
            id: tab_id,
            rel_path: rel_path.to_string(),
            widget: mv.widget.clone(),
            middle: PaneInfo {
                path: mv.middle_tab_path.clone(),
                buf: mv.middle_buf.clone(),
                save: mv.middle_save.clone(),
            },
        });

        let page_num = notebook.append_page(&mv.widget, Some(&tab_label_box));
        notebook.set_current_page(Some(page_num));
        mv.middle_view.grab_focus();

        {
            let nb = notebook.clone();
            let w = mv.widget.clone();
            let ms = mv.middle_save.clone();
            let mtp = mv.middle_tab_path.clone();
            let tabs = open_tabs.clone();
            close_btn.connect_clicked(move |_| {
                if ms.is_sensitive() {
                    if let Some(win) = nb.root().and_downcast::<ApplicationWindow>() {
                        let nb2 = nb.clone();
                        let w2 = w.clone();
                        let tabs2 = tabs.clone();
                        let label = mtp.borrow().clone();
                        confirm_unsaved_dialog(&win, vec![(label, ms.clone())], move || {
                            if let Some(n) = nb2.page_num(&w2) {
                                nb2.remove_page(Some(n));
                            }
                            tabs2.borrow_mut().retain(|t| t.id() != tab_id);
                        });
                    }
                } else if let Some(n) = nb.page_num(&w) {
                    nb.remove_page(Some(n));
                    tabs.borrow_mut().retain(|t| t.id() != tab_id);
                }
            });
        }
    } else {
        let (left_path, right_path) = match plan {
            VcsDiffPlan::WithWorking {
                temp_head,
                working,
                content,
            } => {
                if let Some(parent) = temp_head.parent() {
                    let _ = fs::create_dir_all(parent);
                }
                let _ = fs::write(&temp_head, content);
                (temp_head, working)
            }
            VcsDiffPlan::EmptyWithWorking {
                temp_empty,
                working,
            } => {
                if let Some(parent) = temp_empty.parent() {
                    let _ = fs::create_dir_all(parent);
                }
                let _ = fs::write(&temp_empty, "");
                (temp_empty, working)
            }
            VcsDiffPlan::WithDeleted {
                temp_head,
                temp_deleted,
                content,
            } => {
                for p in [&temp_head, &temp_deleted] {
                    if let Some(parent) = p.parent() {
                        let _ = fs::create_dir_all(parent);
                    }
                }
                let _ = fs::write(&temp_head, content);
                let _ = fs::write(&temp_deleted, "");
                (temp_head, temp_deleted)
            }
            VcsDiffPlan::ConflictMerge { .. } => unreachable!(),
        };

        let labels = vec![format!("HEAD: {rel_path}"), format!("Working: {rel_path}")];
        let dv = build_diff_view(&left_path, &right_path, &labels, settings);
        dv.widget
            .insert_action_group("diff", Some(&dv.action_group));

        open_tabs.borrow_mut().push(FileTab::Diff {
            id: tab_id,
            rel_path: rel_path.to_string(),
            widget: dv.widget.clone(),
            left: PaneInfo {
                path: dv.left_tab_path,
                buf: dv.left_buf,
                save: dv.left_save,
            },
            right: PaneInfo {
                path: dv.right_tab_path,
                buf: dv.right_buf,
                save: dv.right_save,
            },
        });

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
                        tabs.borrow_mut().retain(|t| t.id() != tab_id);
                    }
                }
            });
        }
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
        let encoded = encode_vcs_row(&entry.status, &entry.rel_path, &entry.extra);
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

    let extra_col = ColumnViewColumn::new(Some("Extra"), Some(make_vcs_extra_factory()));
    extra_col.set_fixed_width(130);
    view.append_column(&extra_col);

    let list_scroll = ScrolledWindow::builder().vexpand(true).child(&view).build();

    // Toolbar
    let toolbar = GtkBox::new(Orientation::Horizontal, 8);
    toolbar.set_margin_start(6);
    toolbar.set_margin_end(6);
    toolbar.set_margin_top(4);
    toolbar.set_margin_bottom(4);

    let branch_name = crate::vcs::current_branch(&repo_root);

    let repo_label = Label::new(Some(&repo_root.to_string_lossy()));
    repo_label.set_halign(gtk4::Align::Start);
    repo_label.set_hexpand(true);
    repo_label.add_css_class("dim-label");

    let branch_label = Label::new(Some(branch_name.as_deref().unwrap_or("git")));
    branch_label.add_css_class("dim-label");

    let count_label = Label::new(Some(&format!(
        "{} changed file{}",
        entries.len(),
        if entries.len() == 1 { "" } else { "s" }
    )));
    count_label.add_css_class("chunk-label");

    let refresh_btn = Button::from_icon_name("view-refresh-symbolic");
    refresh_btn.set_tooltip_text(Some("Refresh"));

    let prefs_btn = Button::from_icon_name("preferences-system-symbolic");
    prefs_btn.set_tooltip_text(Some(&format!("Preferences ({}+,)", primary_key_name())));
    prefs_btn.set_action_name(Some("win.prefs"));

    toolbar.append(&repo_label);
    toolbar.append(&branch_label);
    toolbar.append(&count_label);
    toolbar.append(&refresh_btn);
    toolbar.append(&prefs_btn);

    // VCS tab
    let vcs_tab = GtkBox::new(Orientation::Vertical, 0);
    vcs_tab.append(&toolbar);
    vcs_tab.append(&gtk4::Separator::new(Orientation::Horizontal));
    vcs_tab.append(&list_scroll);
    vcs_tab.set_vexpand(true);

    // ── Window via shared helper ─────────────────────────────────────
    let repo_name = repo_root.file_name().map_or_else(
        || repo_root.to_string_lossy().into_owned(),
        |n| n.to_string_lossy().into_owned(),
    );
    let AppWindow {
        window,
        notebook,
        open_tabs,
    } = build_app_window(app, &settings, 700, 500, true);
    let branch_display = branch_name.as_deref().unwrap_or("git");
    window.set_title(Some(&format!("mergers — {repo_name} ({branch_display})")));

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
                let (code, path, _) = decode_vcs_row(&raw);
                open_vcs_diff(&nb, path, code, &tabs, &rr, &td, &st);
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
                .map(|e| encode_vcs_row(&e.status, &e.rel_path, &e.extra))
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

    // File watcher — skip events inside .git/ to avoid infinite refresh loops
    let r = refresh.clone();
    let vcs_watcher = start_file_watcher(
        &[repo_root.as_path()],
        true,
        Some(Box::new(|event: &notify::Event| {
            !event
                .paths
                .iter()
                .all(|p| p.components().any(|c| c.as_os_str() == ".git"))
        })),
        move |fs_dirty| {
            if fs_dirty {
                r();
            }
        },
    );

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
                let (_, rel, _) = decode_vcs_row(&raw);
                let rel = rel.to_string();
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
                            let _ = crate::vcs::discard_changes(&rr, &rel);
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
        let v = view.clone();
        action.connect_activate(move |_, _| {
            if let Some(item) = s.selected_item() {
                let obj = item.downcast::<StringObject>().unwrap();
                let raw = obj.string();
                let (code, path, _) = decode_vcs_row(&raw);
                if code == "C" {
                    let rel = path.to_string();
                    if let Some(win) = v
                        .root()
                        .and_then(|root| root.downcast::<ApplicationWindow>().ok())
                    {
                        let rr = rr.clone();
                        let r = r.clone();
                        show_confirm_dialog(
                            &win,
                            &format!("Stage conflicted file {rel}?"),
                            "This marks the conflict as resolved.",
                            "Stage",
                            move || {
                                let _ = crate::vcs::stage_file(&rr, &rel);
                                r();
                            },
                        );
                    }
                } else {
                    let _ = crate::vcs::stage_file(&rr, path);
                    r();
                }
            }
        });
        vcs_action_group.add_action(&action);
    }
    {
        let action = gio::SimpleAction::new("unstage", None);
        let s = sel.clone();
        let rr = repo_root.clone();
        let r = refresh.clone();
        action.connect_activate(move |_, _| {
            if let Some(item) = s.selected_item() {
                let obj = item.downcast::<StringObject>().unwrap();
                let raw = obj.string();
                let (_, path, _) = decode_vcs_row(&raw);
                let _ = crate::vcs::unstage_file(&rr, path);
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
                let (_, rel, _) = decode_vcs_row(&raw);
                let rel = rel.to_string();
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
                            if let Err(e) = move_to_trash(&path) {
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
    vcs_menu.append(Some("Unstage"), Some("vcs.unstage"));
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
        let act_stage = vcs_action_group
            .lookup_action("stage")
            .and_downcast::<gio::SimpleAction>()
            .unwrap();
        let act_unstage = vcs_action_group
            .lookup_action("unstage")
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
                let (code, _, extra) = decode_vcs_row(&raw);
                let untracked = code == "U";
                let has_staged = !extra.is_empty();
                let fully_staged = extra == "Staged";
                act_open.set_enabled(!untracked);
                act_discard.set_enabled(!untracked);
                act_stage.set_enabled(!untracked && !fully_staged);
                act_unstage.set_enabled(has_staged);
                act_trash.set_enabled(untracked);
                pop.set_pointing_to(Some(&gtk4::gdk::Rectangle::new(x as i32, y as i32, 1, 1)));
                pop.popup();
            }
        });
        view.add_controller(gesture);
    }

    notebook.append_page(&vcs_tab, Some(&Label::new(Some("Changes"))));

    // Clean up temp dir and stop watcher on destroy
    let td = temp_dir.clone();
    window.connect_destroy(move |_| {
        vcs_watcher.alive.set(false);
        let _ = fs::remove_dir_all(&td);
    });

    window.present();
    view.grab_focus();
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_vcs_row_encoding() {
        let status = crate::vcs::VcsStatus::Modified;
        let encoded = encode_vcs_row(&status, "src/main.rs", "Staged");
        let (code, path, extra) = decode_vcs_row(&encoded);
        assert_eq!(code, "M");
        assert_eq!(path, "src/main.rs");
        assert_eq!(extra, "Staged");
    }

    #[test]
    fn test_vcs_status_info() {
        let (label, css) = vcs_status_info("M");
        assert_eq!(label, "Modified");
        assert_eq!(css, "diff-changed");

        let (label, css) = vcs_status_info("U");
        assert_eq!(label, "Untracked");
        assert_eq!(css, "diff-inserted");

        let (label, css) = vcs_status_info("C");
        assert_eq!(label, "Conflict");
        assert_eq!(css, "diff-conflict");

        let (label, css) = vcs_status_info("X");
        assert_eq!(label, "");
        assert_eq!(css, "");
    }

    #[test]
    fn test_plan_vcs_diff_security() {
        let repo = Path::new("/repo");
        let temp = Path::new("/temp");
        // Should refuse paths with ..
        assert_eq!(plan_vcs_diff(repo, temp, "../outside.rs", "M"), None);
    }
}
