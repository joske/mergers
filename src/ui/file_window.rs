#[allow(clippy::wildcard_imports)]
use super::*;

// ─── File comparison window ────────────────────────────────────────────────

pub(super) fn build_file_window(
    app: &Application,
    left_path: std::path::PathBuf,
    right_path: std::path::PathBuf,
    labels: &[String],
    settings: &Rc<RefCell<Settings>>,
) {
    let dv = build_diff_view(&left_path, &right_path, labels, settings);

    // File watcher for both files
    let diff_watcher_alive = Rc::new(Cell::new(true));
    let (fs_tx, fs_rx) = mpsc::channel::<()>();
    let diff_watcher = {
        use notify::{RecursiveMode, Watcher};
        let mut w =
            notify::recommended_watcher(move |res: Result<notify::Event, notify::Error>| {
                if res.is_ok() {
                    let _ = fs_tx.send(());
                }
            })
            .expect("Failed to create file watcher");
        if let Some(parent) = left_path.parent() {
            w.watch(parent, RecursiveMode::NonRecursive).ok();
        }
        if let Some(parent) = right_path.parent() {
            w.watch(parent, RecursiveMode::NonRecursive).ok();
        }
        w
    };

    // Poll for filesystem changes and reload
    {
        let lb = dv.left_buf.clone();
        let rb = dv.right_buf.clone();
        let lp = dv.left_save_path.clone();
        let rp = dv.right_save_path.clone();
        let l_save = dv.left_save.clone();
        let r_save = dv.right_save.clone();
        let alive = diff_watcher_alive.clone();
        let loading = Rc::new(Cell::new(false));
        let dirty = Rc::new(Cell::new(false));
        let retry_count = Rc::new(Cell::new(0u32));
        gtk4::glib::timeout_add_local(Duration::from_millis(500), move || {
            let _ = &diff_watcher; // prevent drop; watcher lives until closure is dropped
            if !alive.get() {
                return gtk4::glib::ControlFlow::Break;
            }
            while fs_rx.try_recv().is_ok() {
                dirty.set(true);
                retry_count.set(0); // new FS event resets the retry counter
            }
            let lp_path = lp.borrow().clone();
            let rp_path = rp.borrow().clone();
            if dirty.get()
                && !loading.get()
                && !is_saving(&[&lp_path, &rp_path])
                && !l_save.is_sensitive()
                && !r_save.is_sensitive()
            {
                dirty.set(false);
                loading.set(true);
                let lb2 = lb.clone();
                let rb2 = rb.clone();
                let l_save2 = l_save.clone();
                let r_save2 = r_save.clone();
                let loading2 = loading.clone();
                let dirty2 = dirty.clone();
                let retry2 = retry_count.clone();
                gtk4::glib::spawn_future_local(async move {
                    let (left_content, right_content) = gio::spawn_blocking(move || {
                        (
                            read_file_for_reload(&lp_path),
                            read_file_for_reload(&rp_path),
                        )
                    })
                    .await
                    .unwrap();
                    loading2.set(false);
                    // Retry on next tick if either file became binary or unreadable
                    let (Some(left_content), Some(right_content)) = (left_content, right_content)
                    else {
                        let n = retry2.get() + 1;
                        retry2.set(n);
                        if n < 5 {
                            dirty2.set(true);
                        } else {
                            eprintln!(
                                "Giving up reload after {n} retries (file unreadable or binary)"
                            );
                        }
                        return;
                    };
                    let cur_left = lb2.text(&lb2.start_iter(), &lb2.end_iter(), false);
                    let cur_right = rb2.text(&rb2.start_iter(), &rb2.end_iter(), false);
                    if cur_left.as_str() != left_content || cur_right.as_str() != right_content {
                        // set_text triggers connect_changed which schedules
                        // refresh_diff with the current filter state.
                        lb2.set_text(&left_content);
                        rb2.set_text(&right_content);
                        l_save2.set_sensitive(false);
                        r_save2.set_sensitive(false);
                    }
                });
            }
            gtk4::glib::ControlFlow::Continue
        });
    }

    // Window title / tab title
    let left_name = if is_blank_path(&left_path) {
        "Untitled".to_string()
    } else {
        left_path.file_name().map_or_else(
            || left_path.display().to_string(),
            |n| n.to_string_lossy().into_owned(),
        )
    };
    let right_name = if is_blank_path(&right_path) {
        "Untitled".to_string()
    } else {
        right_path.file_name().map_or_else(
            || right_path.display().to_string(),
            |n| n.to_string_lossy().into_owned(),
        )
    };
    let title = format!("{left_name} — {right_name}");

    // ── Notebook (tabs) ────────────────────────────────────────────
    let notebook = Notebook::new();
    notebook.set_scrollable(true);
    notebook.append_page(&dv.widget, Some(&Label::new(Some(&title))));

    // Track open file tabs
    let open_tabs: Rc<RefCell<Vec<FileTab>>> = Rc::new(RefCell::new(Vec::new()));
    {
        let tab_id = NEXT_TAB_ID.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        open_tabs.borrow_mut().push(FileTab {
            id: tab_id,
            rel_path: title.clone(),
            widget: dv.widget.clone(),
            left_path: dv.left_tab_path.clone(),
            right_path: dv.right_tab_path.clone(),
            left_buf: dv.left_buf.clone(),
            right_buf: dv.right_buf.clone(),
            left_save: dv.left_save.clone(),
            right_save: dv.right_save.clone(),
        });
    }

    let window = ApplicationWindow::builder()
        .application(app)
        .title("Mergers")
        .default_width(900)
        .default_height(600)
        .child(&notebook)
        .build();
    window.insert_action_group("diff", Some(&dv.action_group));

    // Update tab label when panes are swapped
    {
        let nb = notebook.clone();
        let ln = left_name;
        let rn = right_name;
        let swapped = Rc::new(Cell::new(false));
        *dv.swap_callback.borrow_mut() = Some(Box::new(move || {
            let s = !swapped.get();
            swapped.set(s);
            let new_title = if s {
                format!("{rn} — {ln}")
            } else {
                format!("{ln} — {rn}")
            };
            if let Some(page) = nb.current_page()
                && let Some(child) = nb.nth_page(Some(page))
            {
                nb.set_tab_label_text(&child, &new_title);
            }
        }));
    }

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
    // Close-tab action (Ctrl+W) — close current tab or window
    {
        let action = gio::SimpleAction::new("close-tab", None);
        let nb = notebook.clone();
        let w = window.clone();
        let tabs = open_tabs.clone();
        action.connect_activate(move |_, _| match nb.current_page() {
            None | Some(0) if nb.n_pages() <= 1 => w.close(),
            Some(n) => close_notebook_tab(&w, &nb, &tabs, n),
            None => w.close(),
        });
        win_actions.add_action(&action);
    }
    // New comparison (Ctrl+N)
    {
        let action = gio::SimpleAction::new("new-comparison", None);
        let nb = notebook.clone();
        let st = settings.clone();
        let tabs = open_tabs.clone();
        action.connect_activate(move |_, _| {
            build_new_comparison_tab(&nb, &st, &tabs);
        });
        win_actions.add_action(&action);
    }
    add_tab_navigation_actions(&win_actions, &notebook);
    window.insert_action_group("win", Some(&win_actions));
    add_tab_navigation_keys(&window);

    // Unsaved-changes guard on window close button
    {
        let tabs = open_tabs.clone();
        window.connect_close_request(move |w| handle_notebook_close_request(w, &tabs));
    }

    // Bind keyboard accelerators
    if let Some(gtk_app) = window.application() {
        set_platform_accels(&gtk_app, "diff.prev-chunk", &["<Alt>Up", "<Ctrl>e"]);
        set_platform_accels(&gtk_app, "diff.next-chunk", &["<Alt>Down", "<Ctrl>d"]);
        set_platform_accels(&gtk_app, "diff.find", &["<Ctrl>f"]);
        set_platform_accels(&gtk_app, "diff.find-replace", &["<Ctrl>h"]);
        gtk_app.set_accels_for_action("diff.find-next", &["F3"]);
        gtk_app.set_accels_for_action("diff.find-prev", &["<Shift>F3"]);
        set_platform_accels(&gtk_app, "diff.go-to-line", &["<Ctrl>l"]);
        set_platform_accels(&gtk_app, "diff.export-patch", &["<Ctrl><Shift>p"]);
        set_platform_accels(&gtk_app, "diff.save", &["<Ctrl>s"]);
        set_platform_accels(&gtk_app, "diff.refresh", &["<Ctrl>r"]);
        set_platform_accels(&gtk_app, "diff.open-externally", &["<Ctrl><Shift>o"]);
        set_platform_accels(&gtk_app, "diff.save-as", &["<Ctrl><Shift>s"]);
        set_platform_accels(&gtk_app, "diff.save-all", &["<Ctrl><Shift>l"]);
        set_platform_accels(&gtk_app, "win.prefs", &["<Ctrl>comma"]);
        set_platform_accels(&gtk_app, "win.close-tab", &["<Ctrl>w"]);
        set_platform_accels(&gtk_app, "win.new-comparison", &["<Ctrl>n"]);
    }

    window.connect_destroy(move |_| {
        diff_watcher_alive.set(false);
    });

    window.present();
}
