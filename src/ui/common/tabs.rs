#[allow(clippy::wildcard_imports)]
use super::*;

/// Show a Meld-style "Save changes to documents before closing?" dialog.
///
/// `unsaved` lists (`display_path`, `save_button`) for each file with unsaved
/// changes.  The user can check / uncheck individual files.
///
/// Process unsaved saves one at a time. Synchronous saves (non-blank paths) are
/// handled immediately. Async saves (blank paths via Save As dialog) pause the
/// chain until the save button becomes insensitive, then continue with the rest.
fn process_unsaved_saves(
    checks: &Rc<Vec<(gtk4::CheckButton, Button)>>,
    dialog: &gtk4::Window,
    on_close: &Rc<dyn Fn()>,
) {
    for (check, btn) in checks.iter() {
        if check.is_active() && btn.is_sensitive() {
            btn.emit_clicked();
            if btn.is_sensitive() {
                // Save was async (Save As dialog opened). Wait for completion,
                // then resume processing the remaining saves.
                let checks = checks.clone();
                let dialog = dialog.clone();
                let on_close = on_close.clone();
                let handler_id: Rc<RefCell<Option<gtk4::glib::SignalHandlerId>>> =
                    Rc::new(RefCell::new(None));
                let handler_id2 = handler_id.clone();
                *handler_id.borrow_mut() =
                    Some(btn.connect_notify_local(Some("sensitive"), move |btn, _| {
                        if !btn.is_sensitive() {
                            if let Some(id) = handler_id2.borrow_mut().take() {
                                btn.disconnect(id);
                            }
                            process_unsaved_saves(&checks, &dialog, &on_close);
                        }
                    }));
                return;
            }
            // Sync save succeeded (button now insensitive), continue to next
        }
    }
    // All checked saves completed — close dialog.
    let any_failed = checks
        .iter()
        .any(|(check, btn)| check.is_active() && btn.is_sensitive());
    if !any_failed {
        dialog.close();
        on_close();
    }
}

/// * **Save** — clicks the save button for every checked file, then calls
///   `on_close`.
/// * **Close without Saving** — marks every save button insensitive (so the
///   subsequent close-request won't re-trigger), then calls `on_close`.
/// * **Cancel** — dismisses the dialog; nothing happens.
pub fn confirm_unsaved_dialog(
    parent: &ApplicationWindow,
    unsaved: Vec<(String, Button)>,
    on_close: impl Fn() + 'static,
) {
    let dialog = gtk4::Window::builder()
        .modal(true)
        .transient_for(parent)
        .resizable(false)
        .decorated(true)
        .deletable(false)
        .build();

    let content = GtkBox::new(Orientation::Vertical, 6);
    content.set_margin_top(18);
    content.set_margin_bottom(18);
    content.set_margin_start(18);
    content.set_margin_end(18);

    let title_label = gtk4::Label::new(Some("Save changes to documents before closing?"));
    title_label.add_css_class("title-3");
    title_label.set_margin_bottom(4);
    content.append(&title_label);

    let subtitle = gtk4::Label::new(Some(
        "If you don\u{2019}t save, changes will be permanently lost.",
    ));
    subtitle.set_margin_bottom(8);
    content.append(&subtitle);

    // Build one checkbox per unsaved file.
    let checks: Rc<Vec<(gtk4::CheckButton, Button)>> = Rc::new(
        unsaved
            .into_iter()
            .map(|(path, btn)| {
                let check = gtk4::CheckButton::with_label(&path);
                check.set_active(true);
                content.append(&check);
                (check, btn)
            })
            .collect(),
    );

    // Button row — "Close without Saving" left-aligned, Cancel + Save right.
    let btn_box = GtkBox::new(Orientation::Horizontal, 8);
    btn_box.set_margin_top(14);

    let close_btn = gtk4::Button::with_label("Close without Saving");
    close_btn.add_css_class("destructive-action");

    let spacer = GtkBox::new(Orientation::Horizontal, 0);
    spacer.set_hexpand(true);

    let cancel_btn = gtk4::Button::with_label("Cancel");

    let save_btn = gtk4::Button::with_label("Save");
    save_btn.add_css_class("suggested-action");

    btn_box.append(&close_btn);
    btn_box.append(&spacer);
    btn_box.append(&cancel_btn);
    btn_box.append(&save_btn);
    content.append(&btn_box);

    dialog.set_child(Some(&content));

    let on_close: Rc<dyn Fn()> = Rc::new(on_close);

    // Cancel — just dismiss.
    {
        let d = dialog.clone();
        cancel_btn.connect_clicked(move |_| d.close());
    }
    // Save checked files, then close (unless a save failed).
    // Saves are processed one at a time: if a save is async (Save As for blank
    // panes), we wait for it to complete before processing the next.
    {
        let d = dialog.clone();
        let checks = checks.clone();
        let on_close = on_close.clone();
        save_btn.connect_clicked(move |_| {
            process_unsaved_saves(&checks, &d, &on_close);
        });
    }
    // Close without saving — mark all insensitive so the subsequent
    // close-request handler won't re-prompt.
    {
        let d = dialog.clone();
        let checks = checks.clone();
        let on_close = on_close.clone();
        close_btn.connect_clicked(move |_| {
            for (_, btn) in checks.iter() {
                btn.set_sensitive(false);
            }
            d.close();
            on_close();
        });
    }

    dialog.present();
}

/// Collect unsaved (path, button) pairs from a list of (path, button) where
/// the button is sensitive.
pub fn collect_unsaved(files: Vec<(String, Button)>) -> Vec<(String, Button)> {
    files
        .into_iter()
        .filter(|(_, b)| b.is_sensitive())
        .collect()
}

/// Add tab navigation actions (prev-tab, next-tab, goto-tab) to a window action group.
pub fn add_tab_navigation_actions(win_actions: &gio::SimpleActionGroup, notebook: &Notebook) {
    // Previous tab (Ctrl+Alt+PageUp)
    {
        let action = gio::SimpleAction::new("prev-tab", None);
        let nb = notebook.clone();
        action.connect_activate(move |_, _| {
            if let Some(cur) = nb.current_page()
                && cur > 0
            {
                nb.set_current_page(Some(cur - 1));
            }
        });
        win_actions.add_action(&action);
    }
    // Next tab (Ctrl+Alt+PageDown)
    {
        let action = gio::SimpleAction::new("next-tab", None);
        let nb = notebook.clone();
        action.connect_activate(move |_, _| {
            if let Some(cur) = nb.current_page() {
                let last = nb.n_pages().saturating_sub(1);
                if cur < last {
                    nb.set_current_page(Some(cur + 1));
                }
            }
        });
        win_actions.add_action(&action);
    }
    // Go to tab N (Alt+1-9): expects variant "u" with 0-based index
    {
        let action = gio::SimpleAction::new("goto-tab", Some(gtk4::glib::VariantTy::UINT32));
        let nb = notebook.clone();
        action.connect_activate(move |_, param| {
            if let Some(p) = param {
                let idx = p.get::<u32>().unwrap_or(0);
                if idx < nb.n_pages() {
                    nb.set_current_page(Some(idx));
                }
            }
        });
        win_actions.add_action(&action);
    }
}

/// Map a hardware keycode to a digit (0-8) for Alt+1-9 tab switching.
/// On macOS the keyval is translated by the input method (Alt+1 → ¡) so we
/// must match on the physical keycode instead.
fn keycode_to_tab_index(keycode: u32) -> Option<u32> {
    #[cfg(target_os = "macos")]
    {
        // macOS virtual keycodes for the number row
        match keycode {
            18 => Some(0), // 1
            19 => Some(1), // 2
            20 => Some(2), // 3
            21 => Some(3), // 4
            23 => Some(4), // 5
            22 => Some(5), // 6
            26 => Some(6), // 7
            28 => Some(7), // 8
            25 => Some(8), // 9
            _ => None,
        }
    }
    #[cfg(not(target_os = "macos"))]
    {
        // X11/Wayland keycodes: 10 = 1, 11 = 2, ..., 18 = 9
        if (10..=18).contains(&keycode) {
            Some(keycode - 10)
        } else {
            None
        }
    }
}

/// Install a capture-phase key handler for tab navigation shortcuts.
/// Handles Ctrl+Alt+PageUp/Down and Alt+1-9.
pub fn add_tab_navigation_keys(widget: &impl IsA<gtk4::Widget>) {
    let key_ctl = EventControllerKey::new();
    key_ctl.set_propagation_phase(gtk4::PropagationPhase::Capture);
    key_ctl.connect_key_pressed(move |ctl, key, keycode, mods| {
        // Ctrl+Alt+PageUp/Down — prev/next tab
        if has_primary_modifier(mods) && mods.contains(gtk4::gdk::ModifierType::ALT_MASK) {
            let action_name = match key {
                k if k == gtk4::gdk::Key::Page_Up => Some("prev-tab"),
                k if k == gtk4::gdk::Key::Page_Down => Some("next-tab"),
                _ => None,
            };
            if let Some(name) = action_name {
                if let Some(w) = ctl.widget() {
                    w.activate_action(&format!("win.{name}"), None).ok();
                }
                return gtk4::glib::Propagation::Stop;
            }
        }
        // Alt+1-9 — go to tab by number (use hardware keycode because
        // macOS translates Alt+digit to special characters)
        if mods.contains(gtk4::gdk::ModifierType::ALT_MASK)
            && !has_primary_modifier(mods)
            && let Some(idx) = keycode_to_tab_index(keycode)
        {
            if let Some(w) = ctl.widget() {
                w.activate_action("win.goto-tab", Some(&idx.to_variant()))
                    .ok();
            }
            return gtk4::glib::Propagation::Stop;
        }
        gtk4::glib::Propagation::Proceed
    });
    widget.add_controller(key_ctl);
}

/// Close a notebook file tab, prompting to save if needed.
pub fn close_notebook_tab(
    window: &ApplicationWindow,
    notebook: &Notebook,
    tabs: &Rc<RefCell<Vec<FileTab>>>,
    page: u32,
) {
    let info = tabs.borrow().iter().find_map(|t| {
        if notebook.page_num(t.widget()) == Some(page) {
            let pairs: Vec<(String, Button)> = t
                .saveable_panes()
                .iter()
                .map(|p| {
                    let path = p.path.borrow().clone();
                    let label = if path.is_empty() {
                        "Untitled".to_string()
                    } else {
                        path
                    };
                    (label, p.save.clone())
                })
                .collect();
            Some((t.id(), pairs))
        } else {
            None
        }
    });
    let Some((tab_id, pane_pairs)) = info else {
        notebook.remove_page(Some(page));
        return;
    };
    let unsaved = collect_unsaved(pane_pairs);
    if unsaved.is_empty() {
        notebook.remove_page(Some(page));
        tabs.borrow_mut().retain(|t| t.id() != tab_id);
        return;
    }
    let nb = notebook.clone();
    let tabs = tabs.clone();
    let widget = tabs
        .borrow()
        .iter()
        .find(|t| t.id() == tab_id)
        .map(|t| t.widget().clone());
    confirm_unsaved_dialog(window, unsaved, move || {
        let current_page = widget.as_ref().and_then(|w| nb.page_num(w)).unwrap_or(page);
        nb.remove_page(Some(current_page));
        tabs.borrow_mut().retain(|t| t.id() != tab_id);
    });
}

/// Shared close-request handler for notebook windows (VCS/dir).
pub fn handle_notebook_close_request(
    window: &ApplicationWindow,
    tabs: &Rc<RefCell<Vec<FileTab>>>,
) -> gtk4::glib::Propagation {
    let unsaved: Vec<(String, Button)> = tabs
        .borrow()
        .iter()
        .flat_map(|t| {
            t.saveable_panes()
                .into_iter()
                .filter(|p| p.save.is_sensitive())
                .map(|p| {
                    let path = p.path.borrow().clone();
                    let label = if path.is_empty() {
                        "Untitled".to_string()
                    } else {
                        path
                    };
                    (label, p.save.clone())
                })
                .collect::<Vec<_>>()
        })
        .collect();
    if unsaved.is_empty() {
        return gtk4::glib::Propagation::Proceed;
    }
    let w = window.clone();
    confirm_unsaved_dialog(window, unsaved, move || w.close());
    gtk4::glib::Propagation::Stop
}

/// Create a large button with a title and subtitle, matching the welcome screen style.
pub fn make_welcome_button(title_text: &str, subtitle_text: &str) -> Button {
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

/// Append a "New Comparison" tab to the given notebook with buttons to start
/// file, directory, or 3-way merge comparisons.
///
/// "Compare Files", "Compare Directories", and "3-way Merge" all open
/// comparisons as tabs in the current notebook.
pub fn build_new_comparison_tab(
    notebook: &Notebook,
    settings: &Rc<RefCell<Settings>>,
    open_tabs: &Rc<RefCell<Vec<FileTab>>>,
) {
    let content = GtkBox::new(Orientation::Vertical, 16);
    content.set_margin_top(32);
    content.set_margin_bottom(32);
    content.set_margin_start(48);
    content.set_margin_end(48);
    content.set_valign(gtk4::Align::Center);
    content.set_vexpand(true);

    let title = Label::new(Some("New Comparison"));
    title.add_css_class("title-1");
    content.append(&title);

    let spacer = GtkBox::new(Orientation::Vertical, 0);
    spacer.set_margin_top(8);
    content.append(&spacer);

    let files_btn = make_welcome_button("Compare Files", "Compare two files side-by-side");
    content.append(&files_btn);
    let dirs_btn = make_welcome_button("Compare Directories", "Compare directory trees");
    content.append(&dirs_btn);
    let merge_btn = make_welcome_button("3-way Merge", "Merge three files");
    content.append(&merge_btn);
    let blank_btn = make_welcome_button("Blank Comparison", "Start with empty files");
    content.append(&blank_btn);

    // Tab label with close button
    let tab_label_box = GtkBox::new(Orientation::Horizontal, 4);
    tab_label_box.append(&Label::new(Some("New Comparison")));
    let close_btn = Button::from_icon_name("window-close-symbolic");
    close_btn.add_css_class("flat");
    close_btn.set_margin_start(4);
    tab_label_box.append(&close_btn);

    let page_num = notebook.append_page(&content, Some(&tab_label_box));
    notebook.set_current_page(Some(page_num));

    // Close button on tab label
    {
        let nb = notebook.clone();
        let w = content.clone();
        close_btn.connect_clicked(move |_| {
            if let Some(n) = nb.page_num(&w) {
                nb.remove_page(Some(n));
            }
        });
    }

    // Compare Files handler — opens as a tab in the current notebook
    {
        let nb = notebook.clone();
        let st = settings.clone();
        let w = content.clone();
        let tabs = open_tabs.clone();
        files_btn.connect_clicked(move |btn| {
            let win = btn
                .root()
                .and_downcast::<ApplicationWindow>()
                .expect("button must be in a window");
            let dialog = gtk4::FileDialog::new();
            dialog.set_title("Select first file");
            let st2 = st.clone();
            let nb2 = nb.clone();
            let w2 = w.clone();
            let tabs2 = tabs.clone();
            dialog.open(Some(&win), gio::Cancellable::NONE, move |result| {
                if let Ok(first) = result
                    && let Some(first_path) = first.path()
                {
                    let dialog2 = gtk4::FileDialog::new();
                    dialog2.set_title("Select second file");
                    let st3 = st2.clone();
                    let nb3 = nb2.clone();
                    let w3 = w2.clone();
                    let tabs3 = tabs2.clone();
                    let win2 = nb2.root().and_downcast::<ApplicationWindow>().unwrap();
                    dialog2.open(Some(&win2), gio::Cancellable::NONE, move |result2| {
                        if let Ok(second) = result2
                            && let Some(second_path) = second.path()
                        {
                            open_file_diff_paths(&nb3, first_path, second_path, &tabs3, &st3);
                            if let Some(n) = nb3.page_num(&w3) {
                                nb3.remove_page(Some(n));
                            }
                        }
                    });
                }
            });
        });
    }

    // Compare Directories handler — opens as a tab in the current notebook
    {
        let nb = notebook.clone();
        let st = settings.clone();
        let w = content.clone();
        let tabs = open_tabs.clone();
        dirs_btn.connect_clicked(move |btn| {
            let win = btn
                .root()
                .and_downcast::<ApplicationWindow>()
                .expect("button must be in a window");
            let dialog = gtk4::FileDialog::new();
            dialog.set_title("Select first directory");
            let st2 = st.clone();
            let nb2 = nb.clone();
            let w2 = w.clone();
            let tabs2 = tabs.clone();
            dialog.select_folder(Some(&win), gio::Cancellable::NONE, move |result| {
                if let Ok(first) = result
                    && let Some(first_path) = first.path()
                {
                    let dialog2 = gtk4::FileDialog::new();
                    dialog2.set_title("Select second directory");
                    let st3 = st2.clone();
                    let nb3 = nb2.clone();
                    let w3 = w2.clone();
                    let tabs3 = tabs2.clone();
                    let win2 = nb2.root().and_downcast::<ApplicationWindow>().unwrap();
                    dialog2.select_folder(Some(&win2), gio::Cancellable::NONE, move |result2| {
                        if let Ok(second) = result2
                            && let Some(second_path) = second.path()
                        {
                            let (dir_widget, dir_watcher, left_view, dir_title) =
                                build_dir_tab(first_path, second_path, &[], st3, &nb3, &tabs3);

                            // Tab label with close button
                            let tab_label_box = GtkBox::new(Orientation::Horizontal, 4);
                            tab_label_box.append(&Label::new(Some(&dir_title)));
                            let close_btn = Button::from_icon_name("window-close-symbolic");
                            close_btn.set_has_frame(false);
                            tab_label_box.append(&close_btn);

                            let page_num = nb3.append_page(&dir_widget, Some(&tab_label_box));
                            nb3.set_current_page(Some(page_num));
                            left_view.grab_focus();

                            // Stop watcher when tab is removed by any means
                            // (close button, Ctrl+W, window close)
                            {
                                let dw2 = dir_widget.clone();
                                let wa = dir_watcher.alive.clone();
                                nb3.connect_page_removed(move |_, child, _| {
                                    if *child == dw2 {
                                        wa.set(false);
                                    }
                                });
                            }

                            {
                                let nb4 = nb3.clone();
                                let dw = dir_widget.clone();
                                close_btn.connect_clicked(move |_| {
                                    if let Some(n) = nb4.page_num(&dw) {
                                        nb4.remove_page(Some(n));
                                    }
                                });
                            }

                            // Remove the New Comparison tab
                            if let Some(n) = nb3.page_num(&w3) {
                                nb3.remove_page(Some(n));
                            }
                        }
                    });
                }
            });
        });
    }

    // 3-way Merge handler — opens as a tab in the current notebook
    {
        let nb = notebook.clone();
        let st = settings.clone();
        let w = content.clone();
        let tabs = open_tabs.clone();
        merge_btn.connect_clicked(move |btn| {
            let win = btn
                .root()
                .and_downcast::<ApplicationWindow>()
                .expect("button must be in a window");
            let dialog = gtk4::FileDialog::new();
            dialog.set_title("Select left file");
            let st2 = st.clone();
            let nb2 = nb.clone();
            let w2 = w.clone();
            let tabs2 = tabs.clone();
            dialog.open(Some(&win), gio::Cancellable::NONE, move |result| {
                if let Ok(first) = result
                    && let Some(first_path) = first.path()
                {
                    let dialog2 = gtk4::FileDialog::new();
                    dialog2.set_title("Select middle (base) file");
                    let st3 = st2.clone();
                    let nb3 = nb2.clone();
                    let w3 = w2.clone();
                    let tabs3 = tabs2.clone();
                    let win2 = nb2.root().and_downcast::<ApplicationWindow>().unwrap();
                    dialog2.open(Some(&win2), gio::Cancellable::NONE, move |result2| {
                        if let Ok(second) = result2
                            && let Some(second_path) = second.path()
                        {
                            let dialog3 = gtk4::FileDialog::new();
                            dialog3.set_title("Select right file");
                            let st4 = st3.clone();
                            let nb4 = nb3.clone();
                            let w4 = w3.clone();
                            let tabs4 = tabs3.clone();
                            let win3 = nb3.root().and_downcast::<ApplicationWindow>().unwrap();
                            dialog3.open(Some(&win3), gio::Cancellable::NONE, move |result3| {
                                if let Ok(third) = result3
                                    && let Some(third_path) = third.path()
                                {
                                    let mv = build_merge_view(
                                        &first_path,
                                        &second_path,
                                        &third_path,
                                        &[],
                                        &st4,
                                    );
                                    mv.widget
                                        .insert_action_group("diff", Some(&mv.action_group));

                                    // Build tab title
                                    let ln = first_path.file_name().map_or_else(
                                        || first_path.display().to_string(),
                                        |n| n.to_string_lossy().into_owned(),
                                    );
                                    let mn = second_path.file_name().map_or_else(
                                        || second_path.display().to_string(),
                                        |n| n.to_string_lossy().into_owned(),
                                    );
                                    let rn = third_path.file_name().map_or_else(
                                        || third_path.display().to_string(),
                                        |n| n.to_string_lossy().into_owned(),
                                    );
                                    let merge_title = format!("{ln} — {mn} — {rn}");

                                    // Tab label with close button
                                    let tab_label_box = GtkBox::new(Orientation::Horizontal, 4);
                                    tab_label_box.append(&Label::new(Some(&merge_title)));
                                    let close_btn = Button::from_icon_name("window-close-symbolic");
                                    close_btn.set_has_frame(false);
                                    tab_label_box.append(&close_btn);

                                    let page_num =
                                        nb4.append_page(&mv.widget, Some(&tab_label_box));
                                    nb4.set_current_page(Some(page_num));

                                    // Register in open_tabs so window-close checks unsaved
                                    let tab_id = NEXT_TAB_ID
                                        .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                                    tabs4.borrow_mut().push(FileTab::Merge {
                                        id: tab_id,
                                        rel_path: merge_title,
                                        widget: mv.widget.clone(),
                                        middle: PaneInfo {
                                            path: mv.middle_tab_path.clone(),
                                            buf: mv.middle_buf.clone(),
                                            save: mv.middle_save.clone(),
                                        },
                                    });

                                    {
                                        let nb5 = nb4.clone();
                                        let mw = mv.widget.clone();
                                        let ms = mv.middle_save.clone();
                                        let mtp = mv.middle_tab_path.clone();
                                        let tabs5 = tabs4.clone();
                                        close_btn.connect_clicked(move |_| {
                                            // Check unsaved merge before closing
                                            if ms.is_sensitive() {
                                                if let Some(win) =
                                                    nb5.root().and_downcast::<ApplicationWindow>()
                                                {
                                                    let nb6 = nb5.clone();
                                                    let mw2 = mw.clone();
                                                    let tabs6 = tabs5.clone();
                                                    let label = mtp.borrow().clone();
                                                    confirm_unsaved_dialog(
                                                        &win,
                                                        vec![(label, ms.clone())],
                                                        move || {
                                                            if let Some(n) = nb6.page_num(&mw2) {
                                                                nb6.remove_page(Some(n));
                                                            }
                                                            tabs6
                                                                .borrow_mut()
                                                                .retain(|t| t.id() != tab_id);
                                                        },
                                                    );
                                                }
                                                return;
                                            }
                                            if let Some(n) = nb5.page_num(&mw) {
                                                nb5.remove_page(Some(n));
                                            }
                                            tabs5.borrow_mut().retain(|t| t.id() != tab_id);
                                        });
                                    }

                                    // Remove the New Comparison tab
                                    if let Some(n) = nb4.page_num(&w4) {
                                        nb4.remove_page(Some(n));
                                    }
                                }
                            });
                        }
                    });
                }
            });
        });
    }

    // Blank Comparison handler — opens an empty diff tab
    {
        let nb = notebook.clone();
        let st = settings.clone();
        let w = content.clone();
        let tabs = open_tabs.clone();
        blank_btn.connect_clicked(move |_| {
            open_blank_diff(&nb, &tabs, &st);
            if let Some(n) = nb.page_num(&w) {
                nb.remove_page(Some(n));
            }
        });
    }
}

pub struct AppWindow {
    pub window: ApplicationWindow,
    pub notebook: Notebook,
    pub open_tabs: Rc<RefCell<Vec<FileTab>>>,
}

/// Create an `ApplicationWindow` with a `Notebook`, shared win actions
/// (prefs, close-tab, new-comparison, tab navigation), close-request handler,
/// and the superset of all keyboard accelerators.
///
/// When `pinned_first_tab` is true, Ctrl+W on page 0 always closes the window.
/// When false (file window), Ctrl+W on page 0 uses `close_notebook_tab` if
/// there are other tabs open.
pub fn build_app_window(
    app: &Application,
    settings: &Rc<RefCell<Settings>>,
    default_width: i32,
    default_height: i32,
    pinned_first_tab: bool,
) -> AppWindow {
    let notebook = Notebook::new();
    notebook.set_scrollable(true);

    let open_tabs: Rc<RefCell<Vec<FileTab>>> = Rc::new(RefCell::new(Vec::new()));

    let window = ApplicationWindow::builder()
        .application(app)
        .title("Mergers")
        .default_width(default_width)
        .default_height(default_height)
        .child(&notebook)
        .build();

    // ── Win actions ──────────────────────────────────────────────────
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
    {
        let action = gio::SimpleAction::new("close-tab", None);
        let nb = notebook.clone();
        let w = window.clone();
        let tabs = open_tabs.clone();
        action.connect_activate(move |_, _| match nb.current_page() {
            Some(0) | None if pinned_first_tab => w.close(),
            None | Some(0) if nb.n_pages() <= 1 => w.close(),
            Some(n) => close_notebook_tab(&w, &nb, &tabs, n),
            None => w.close(),
        });
        win_actions.add_action(&action);
    }
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

    // ── Unsaved-changes guard ────────────────────────────────────────
    {
        let tabs = open_tabs.clone();
        window.connect_close_request(move |w| handle_notebook_close_request(w, &tabs));
    }

    // ── Keyboard accelerators (superset of all window types) ─────────
    if let Some(gtk_app) = window.application() {
        // Diff actions (active when a diff/merge tab is focused)
        set_platform_accels(&gtk_app, "diff.prev-chunk", &["<Alt>Up", "<Ctrl>e"]);
        set_platform_accels(&gtk_app, "diff.next-chunk", &["<Alt>Down", "<Ctrl>d"]);
        set_platform_accels(&gtk_app, "diff.find", &["<Ctrl>f"]);
        if cfg!(target_os = "macos") {
            // Cmd+H is the system "Hide" shortcut on macOS; use Cmd+Shift+H instead
            gtk_app.set_accels_for_action("diff.find-replace", &["<Meta><Shift>h"]);
        } else {
            gtk_app.set_accels_for_action("diff.find-replace", &["<Ctrl>h"]);
        }
        gtk_app.set_accels_for_action("diff.find-next", &["F3"]);
        gtk_app.set_accels_for_action("diff.find-prev", &["<Shift>F3"]);
        set_platform_accels(&gtk_app, "diff.go-to-line", &["<Ctrl>l"]);
        set_platform_accels(&gtk_app, "diff.export-patch", &["<Ctrl><Shift>p"]);
        set_platform_accels(&gtk_app, "diff.save", &["<Ctrl>s"]);
        set_platform_accels(&gtk_app, "diff.refresh", &["<Ctrl>r"]);
        set_platform_accels(&gtk_app, "diff.open-externally", &["<Ctrl><Shift>o"]);
        set_platform_accels(&gtk_app, "diff.save-as", &["<Ctrl><Shift>s"]);
        set_platform_accels(&gtk_app, "diff.save-all", &["<Ctrl><Shift>l"]);
        // Merge-specific diff actions
        set_platform_accels(&gtk_app, "diff.prev-conflict", &["<Ctrl>j"]);
        set_platform_accels(&gtk_app, "diff.next-conflict", &["<Ctrl>k"]);
        // Win actions
        set_platform_accels(&gtk_app, "win.prefs", &["<Ctrl>comma"]);
        set_platform_accels(&gtk_app, "win.close-tab", &["<Ctrl>w"]);
        set_platform_accels(&gtk_app, "win.new-comparison", &["<Ctrl>n"]);
    }

    AppWindow {
        window,
        notebook,
        open_tabs,
    }
}
