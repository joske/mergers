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
        let lp = left_path.clone();
        let rp = right_path.clone();
        let l_save = dv.left_save.clone();
        let r_save = dv.right_save.clone();
        let alive = diff_watcher_alive.clone();
        let loading = Rc::new(Cell::new(false));
        let dirty = Rc::new(Cell::new(false));
        gtk4::glib::timeout_add_local(Duration::from_millis(500), move || {
            let _ = &diff_watcher; // prevent drop; watcher lives until closure is dropped
            if !alive.get() {
                return gtk4::glib::ControlFlow::Break;
            }
            while fs_rx.try_recv().is_ok() {
                dirty.set(true);
            }
            if dirty.get()
                && !loading.get()
                && !is_saving(&[&lp, &rp])
                && !l_save.is_sensitive()
                && !r_save.is_sensitive()
            {
                dirty.set(false);
                loading.set(true);
                let lp2 = lp.clone();
                let rp2 = rp.clone();
                let lb2 = lb.clone();
                let rb2 = rb.clone();
                let l_save2 = l_save.clone();
                let r_save2 = r_save.clone();
                let loading2 = loading.clone();
                let dirty2 = dirty.clone();
                gtk4::glib::spawn_future_local(async move {
                    let (left_content, right_content) = gio::spawn_blocking(move || {
                        (read_file_for_reload(&lp2), read_file_for_reload(&rp2))
                    })
                    .await
                    .unwrap();
                    loading2.set(false);
                    // Retry on next tick if either file became binary or unreadable
                    let (Some(left_content), Some(right_content)) = (left_content, right_content)
                    else {
                        dirty2.set(true);
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
    // Close-tab action (Ctrl+W) — close file window
    {
        let action = gio::SimpleAction::new("close-tab", None);
        let w = window.clone();
        action.connect_activate(move |_, _| w.close());
        win_actions.add_action(&action);
    }
    window.insert_action_group("win", Some(&win_actions));

    // Unsaved-changes guard on window close button
    {
        let ls = dv.left_save.clone();
        let rs = dv.right_save.clone();
        let lp = left_path.display().to_string();
        let rp = right_path.display().to_string();
        window.connect_close_request(move |w| {
            handle_close_request(w, vec![(lp.clone(), ls.clone()), (rp.clone(), rs.clone())])
        });
    }

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
        gtk_app.set_accels_for_action("win.close-tab", &["<Ctrl>w"]);
    }

    window.connect_destroy(move |_| {
        diff_watcher_alive.set(false);
    });

    window.present();
}
