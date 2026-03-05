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
    let watch_paths: Vec<&Path> = [left_path.parent(), right_path.parent()]
        .iter()
        .filter_map(|p| *p)
        .collect();
    let lb = dv.left_buf.clone();
    let rb = dv.right_buf.clone();
    let lp = dv.left_save_path.clone();
    let rp = dv.right_save_path.clone();
    let l_save = dv.left_save.clone();
    let r_save = dv.right_save.clone();
    let loading = Rc::new(Cell::new(false));
    let dirty = Rc::new(Cell::new(false));
    let retry_count = Rc::new(Cell::new(0u32));
    let diff_watcher = start_file_watcher(&watch_paths, false, None, move |fs_dirty| {
        if fs_dirty {
            dirty.set(true);
            retry_count.set(0);
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
                let (Some(left_content), Some(right_content)) = (left_content, right_content)
                else {
                    let n = retry2.get() + 1;
                    retry2.set(n);
                    if n < 5 {
                        dirty2.set(true);
                    } else {
                        eprintln!("Giving up reload after {n} retries (file unreadable or binary)");
                    }
                    return;
                };
                let cur_left = lb2.text(&lb2.start_iter(), &lb2.end_iter(), false);
                let cur_right = rb2.text(&rb2.start_iter(), &rb2.end_iter(), false);
                if cur_left.as_str() != left_content || cur_right.as_str() != right_content {
                    lb2.set_text(&left_content);
                    rb2.set_text(&right_content);
                    l_save2.set_sensitive(false);
                    r_save2.set_sensitive(false);
                }
            });
        }
    });

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

    // ── Window via shared helper ─────────────────────────────────────
    let AppWindow {
        window,
        notebook,
        open_tabs,
    } = build_app_window(app, settings, 900, 600, false);

    dv.widget
        .insert_action_group("diff", Some(&dv.action_group));
    notebook.append_page(&dv.widget, Some(&Label::new(Some(&title))));

    // Register initial tab
    {
        let tab_id = NEXT_TAB_ID.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        open_tabs.borrow_mut().push(FileTab::Diff {
            id: tab_id,
            rel_path: title.clone(),
            widget: dv.widget.clone(),
            left: PaneInfo {
                path: dv.left_tab_path.clone(),
                buf: dv.left_buf.clone(),
                save: dv.left_save.clone(),
            },
            right: PaneInfo {
                path: dv.right_tab_path.clone(),
                buf: dv.right_buf.clone(),
                save: dv.right_save.clone(),
            },
        });
    }

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

    window.connect_destroy(move |_| {
        diff_watcher.alive.set(false);
    });

    window.present();
}
