#[allow(clippy::wildcard_imports)]
use super::*;

// ─── Welcome window ─────────────────────────────────────────────────────────

pub(super) fn build_welcome_window(app: &Application, settings: Rc<RefCell<Settings>>) {
    let window = ApplicationWindow::builder()
        .application(app)
        .title("merde")
        .default_width(480)
        .default_height(360)
        .build();

    let content = GtkBox::new(Orientation::Vertical, 16);
    content.set_margin_top(32);
    content.set_margin_bottom(32);
    content.set_margin_start(48);
    content.set_margin_end(48);
    content.set_valign(gtk4::Align::Center);

    let title = Label::new(Some("merde"));
    title.add_css_class("title-1");
    content.append(&title);

    let subtitle = Label::new(Some("Visual Diff and Merge Tool"));
    subtitle.add_css_class("dim-label");
    content.append(&subtitle);

    let spacer = GtkBox::new(Orientation::Vertical, 0);
    spacer.set_margin_top(8);
    content.append(&spacer);

    // Compare Files button
    let files_btn = make_welcome_button("Compare Files", "Compare two files side-by-side");
    content.append(&files_btn);

    // Compare Directories button
    let dirs_btn = make_welcome_button("Compare Directories", "Compare directory trees");
    content.append(&dirs_btn);

    // 3-way Merge button
    let merge_btn = make_welcome_button("3-way Merge", "Merge three files");
    content.append(&merge_btn);

    // Compare Files handler
    {
        let app2 = app.clone();
        let w = window.clone();
        let st = settings.clone();
        files_btn.connect_clicked(move |_| {
            let dialog = gtk4::FileDialog::new();
            dialog.set_title("Select first file");
            let app3 = app2.clone();
            let w2 = w.clone();
            let st2 = st.clone();
            dialog.open(Some(&w), gio::Cancellable::NONE, move |result| {
                if let Ok(first) = result
                    && let Some(first_path) = first.path()
                {
                    let dialog2 = gtk4::FileDialog::new();
                    dialog2.set_title("Select second file");
                    let app4 = app3.clone();
                    let w3 = w2.clone();
                    let st3 = st2.clone();
                    dialog2.open(Some(&w2), gio::Cancellable::NONE, move |result2| {
                        if let Ok(second) = result2
                            && let Some(second_path) = second.path()
                        {
                            build_file_window(&app4, first_path, second_path, &[], &st3);
                            w3.close();
                        }
                    });
                }
            });
        });
    }

    // Compare Directories handler
    {
        let app2 = app.clone();
        let w = window.clone();
        let st = settings.clone();
        dirs_btn.connect_clicked(move |_| {
            let dialog = gtk4::FileDialog::new();
            dialog.set_title("Select first directory");
            let app3 = app2.clone();
            let w2 = w.clone();
            let st2 = st.clone();
            dialog.select_folder(Some(&w), gio::Cancellable::NONE, move |result| {
                if let Ok(first) = result
                    && let Some(first_path) = first.path()
                {
                    let dialog2 = gtk4::FileDialog::new();
                    dialog2.set_title("Select second directory");
                    let app4 = app3.clone();
                    let w3 = w2.clone();
                    let st3 = st2.clone();
                    dialog2.select_folder(Some(&w2), gio::Cancellable::NONE, move |result2| {
                        if let Ok(second) = result2
                            && let Some(second_path) = second.path()
                        {
                            build_dir_window(&app4, first_path, second_path, st3);
                            w3.close();
                        }
                    });
                }
            });
        });
    }

    // 3-way Merge handler
    {
        let app2 = app.clone();
        let w = window.clone();
        let st = settings.clone();
        merge_btn.connect_clicked(move |_| {
            let dialog = gtk4::FileDialog::new();
            dialog.set_title("Select left file");
            let app3 = app2.clone();
            let w2 = w.clone();
            let st2 = st.clone();
            dialog.open(Some(&w), gio::Cancellable::NONE, move |result| {
                if let Ok(first) = result
                    && let Some(first_path) = first.path()
                {
                    let dialog2 = gtk4::FileDialog::new();
                    dialog2.set_title("Select middle (base) file");
                    let app4 = app3.clone();
                    let w3 = w2.clone();
                    let st3 = st2.clone();
                    dialog2.open(Some(&w2), gio::Cancellable::NONE, move |result2| {
                        if let Ok(second) = result2
                            && let Some(second_path) = second.path()
                        {
                            let dialog3 = gtk4::FileDialog::new();
                            dialog3.set_title("Select right file");
                            let app5 = app4.clone();
                            let w4 = w3.clone();
                            let st4 = st3.clone();
                            dialog3.open(Some(&w3), gio::Cancellable::NONE, move |result3| {
                                if let Ok(third) = result3
                                    && let Some(third_path) = third.path()
                                {
                                    build_merge_window(
                                        &app5,
                                        first_path,
                                        second_path,
                                        third_path,
                                        &[],
                                        &st4,
                                    );
                                    w4.close();
                                }
                            });
                        }
                    });
                }
            });
        });
    }

    // Preferences
    let prefs_btn = Button::from_icon_name("preferences-system-symbolic");
    prefs_btn.set_tooltip_text(Some("Preferences (Ctrl+,)"));
    prefs_btn.set_action_name(Some("win.prefs"));
    prefs_btn.set_halign(gtk4::Align::End);
    prefs_btn.set_margin_top(8);
    content.append(&prefs_btn);

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
    // Close-tab action (Ctrl+W) — close welcome window
    {
        let action = gio::SimpleAction::new("close-tab", None);
        let w = window.clone();
        action.connect_activate(move |_, _| {
            w.close();
        });
        win_actions.add_action(&action);
    }
    window.insert_action_group("win", Some(&win_actions));

    if let Some(gtk_app) = window.application() {
        gtk_app.set_accels_for_action("win.prefs", &["<Ctrl>comma"]);
        gtk_app.set_accels_for_action("win.close-tab", &["<Ctrl>w"]);
    }

    window.set_child(Some(&content));
    window.present();
}

fn make_welcome_button(title_text: &str, subtitle_text: &str) -> Button {
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
