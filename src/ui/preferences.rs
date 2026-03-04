#[allow(clippy::wildcard_imports)]
use super::*;

// ─── Preferences dialog ────────────────────────────────────────────────────

fn apply_settings_to_views(window: &gtk4::Window, settings: &Settings) {
    // Walk all sourceview5::View widgets in the window and re-apply settings
    fn walk(widget: &gtk4::Widget, settings: &Settings) {
        if widget.is::<sourceview5::View>() {
            let sv: sourceview5::View = widget.clone().downcast().unwrap();
            sv.set_show_line_numbers(settings.show_line_numbers);
            sv.set_highlight_current_line(settings.highlight_current_line);
            sv.set_wrap_mode(settings.wrap_mode_gtk());
            sv.set_tab_width(settings.tab_width);
            sv.set_insert_spaces_instead_of_tabs(settings.insert_spaces);
            {
                let drawer = sv.space_drawer();
                if settings.show_whitespace {
                    drawer.set_types_for_locations(
                        sourceview5::SpaceLocationFlags::ALL,
                        sourceview5::SpaceTypeFlags::ALL,
                    );
                    drawer.set_enable_matrix(true);
                } else {
                    drawer.set_enable_matrix(false);
                }
            }
            // Update conflict mark attributes for the new scheme colours.
            setup_conflict_marks(&sv);
            let buf: TextBuffer = sv.buffer();
            if let Ok(sbuf) = buf.downcast::<sourceview5::Buffer>() {
                let scheme_mgr = sourceview5::StyleSchemeManager::default();
                if let Some(scheme) = scheme_mgr.scheme(&settings.style_scheme) {
                    sbuf.set_style_scheme(Some(&scheme));
                }
                update_diff_tag_colors(sbuf.upcast_ref());
            }
        }
        // Recurse into children
        let mut child = widget.first_child();
        while let Some(c) = child {
            walk(&c, settings);
            child = c.next_sibling();
        }
    }
    update_font_css(settings);
    // Update scheme CSS first so IS_DARK_SCHEME is set before tag colour updates.
    apply_scheme_css(settings);

    let w: gtk4::Widget = window.clone().upcast();
    walk(&w, settings);
}

fn make_pref_row(label_text: &str, widget: &impl IsA<gtk4::Widget>) -> GtkBox {
    let row = GtkBox::new(Orientation::Horizontal, 12);
    row.set_margin_start(12);
    row.set_margin_end(12);
    row.set_margin_top(4);
    row.set_margin_bottom(4);
    let label = Label::new(Some(label_text));
    label.set_halign(gtk4::Align::Start);
    label.set_hexpand(true);
    row.append(&label);
    row.append(widget);
    row
}

pub(super) fn show_preferences(parent: &ApplicationWindow, settings: &Rc<RefCell<Settings>>) {
    let win = gtk4::Window::builder()
        .title("Preferences")
        .transient_for(parent)
        .modal(true)
        .default_width(500)
        .default_height(450)
        .build();

    let w = win.clone();
    let key_ctl = EventControllerKey::new();
    key_ctl.connect_key_pressed(move |_, key, _, _| {
        if key == gtk4::gdk::Key::Escape {
            w.close();
            return gtk4::glib::Propagation::Stop;
        }
        gtk4::glib::Propagation::Proceed
    });
    win.add_controller(key_ctl);

    let content = GtkBox::new(Orientation::Vertical, 0);
    content.set_margin_top(8);
    content.set_margin_bottom(8);

    // ── Editor section header ──
    let editor_header = Label::new(Some("Editor"));
    editor_header.set_halign(gtk4::Align::Start);
    editor_header.set_margin_start(12);
    editor_header.set_margin_top(8);
    editor_header.set_margin_bottom(4);
    editor_header.add_css_class("heading");
    content.append(&editor_header);

    let s = settings.borrow();

    // Font
    let font_desc = gtk4::pango::FontDescription::from_string(&s.font);
    let font_dialog = gtk4::FontDialog::new();
    let font_btn = gtk4::FontDialogButton::new(Some(font_dialog));
    font_btn.set_font_desc(&font_desc);
    content.append(&make_pref_row("Font", &font_btn));

    // Style scheme
    let scheme_mgr = sourceview5::StyleSchemeManager::default();
    let scheme_ids = scheme_mgr.scheme_ids();
    let scheme_strings: Vec<String> = scheme_ids
        .iter()
        .map(std::string::ToString::to_string)
        .collect();
    let scheme_strs: Vec<&str> = scheme_strings.iter().map(String::as_str).collect();
    let scheme_list = gtk4::StringList::new(&scheme_strs);
    let scheme_dropdown = gtk4::DropDown::new(Some(scheme_list), gtk4::Expression::NONE);
    if let Some(pos) = scheme_strings
        .iter()
        .position(|id| id.eq_ignore_ascii_case(&s.style_scheme))
    {
        scheme_dropdown.set_selected(pos as u32);
    }
    content.append(&make_pref_row("Color scheme", &scheme_dropdown));

    // Show line numbers
    let line_num_switch = gtk4::Switch::new();
    line_num_switch.set_active(s.show_line_numbers);
    line_num_switch.set_valign(gtk4::Align::Center);
    content.append(&make_pref_row("Show line numbers", &line_num_switch));

    // Highlight current line
    let highlight_switch = gtk4::Switch::new();
    highlight_switch.set_active(s.highlight_current_line);
    highlight_switch.set_valign(gtk4::Align::Center);
    content.append(&make_pref_row("Highlight current line", &highlight_switch));

    // Wrap around navigation
    let wrap_nav_switch = gtk4::Switch::new();
    wrap_nav_switch.set_active(s.wrap_around_navigation);
    wrap_nav_switch.set_valign(gtk4::Align::Center);
    content.append(&make_pref_row("Wrap around navigation", &wrap_nav_switch));

    // Word wrap
    let wrap_modes = gtk4::StringList::new(&["None", "Word", "Character"]);
    let wrap_dropdown = gtk4::DropDown::new(Some(wrap_modes), gtk4::Expression::NONE);
    wrap_dropdown.set_selected(match s.wrap_mode.as_str() {
        "word" => 1,
        "char" => 2,
        _ => 0,
    });
    content.append(&make_pref_row("Word wrap", &wrap_dropdown));

    // Tab width
    let tab_adj = Adjustment::new(f64::from(s.tab_width), 1.0, 16.0, 1.0, 4.0, 0.0);
    let tab_spin = gtk4::SpinButton::new(Some(&tab_adj), 1.0, 0);
    content.append(&make_pref_row("Tab width", &tab_spin));

    // Insert spaces instead of tabs
    let insert_spaces_switch = gtk4::Switch::new();
    insert_spaces_switch.set_active(s.insert_spaces);
    insert_spaces_switch.set_valign(gtk4::Align::Center);
    content.append(&make_pref_row(
        "Insert spaces instead of tabs",
        &insert_spaces_switch,
    ));

    // Show whitespace
    let show_ws_switch = gtk4::Switch::new();
    show_ws_switch.set_active(s.show_whitespace);
    show_ws_switch.set_valign(gtk4::Align::Center);
    content.append(&make_pref_row("Show whitespace", &show_ws_switch));

    // ── Comparison section ──
    let cmp_header = Label::new(Some("Comparison"));
    cmp_header.set_halign(gtk4::Align::Start);
    cmp_header.set_margin_start(12);
    cmp_header.set_margin_top(16);
    cmp_header.set_margin_bottom(4);
    cmp_header.add_css_class("heading");
    content.append(&cmp_header);

    let ignore_blanks_switch = gtk4::Switch::new();
    ignore_blanks_switch.set_active(s.ignore_blank_lines);
    ignore_blanks_switch.set_valign(gtk4::Align::Center);
    content.append(&make_pref_row("Ignore blank lines", &ignore_blanks_switch));

    let ignore_ws_switch = gtk4::Switch::new();
    ignore_ws_switch.set_active(s.ignore_whitespace);
    ignore_ws_switch.set_valign(gtk4::Align::Center);
    content.append(&make_pref_row(
        "Ignore whitespace differences",
        &ignore_ws_switch,
    ));

    // ── Directory section ──
    let dir_header = Label::new(Some("Directory"));
    dir_header.set_halign(gtk4::Align::Start);
    dir_header.set_margin_start(12);
    dir_header.set_margin_top(16);
    dir_header.set_margin_bottom(4);
    dir_header.add_css_class("heading");
    content.append(&dir_header);

    // Hide hidden files
    let hidden_switch = gtk4::Switch::new();
    hidden_switch.set_active(s.hide_hidden_files);
    hidden_switch.set_valign(gtk4::Align::Center);
    content.append(&make_pref_row("Hide hidden files", &hidden_switch));

    // ── File Filters section ──
    let filter_header = Label::new(Some("File Filters"));
    filter_header.set_halign(gtk4::Align::Start);
    filter_header.set_margin_start(12);
    filter_header.set_margin_top(16);
    filter_header.set_margin_bottom(4);
    filter_header.add_css_class("heading");
    content.append(&filter_header);

    let filter_desc = Label::new(Some("Names to exclude from directory comparison"));
    filter_desc.set_halign(gtk4::Align::Start);
    filter_desc.set_margin_start(12);
    filter_desc.add_css_class("dim-label");
    content.append(&filter_desc);

    let filter_list_box = GtkBox::new(Orientation::Vertical, 2);
    filter_list_box.set_margin_top(4);
    filter_list_box.set_margin_bottom(4);
    filter_list_box.set_margin_start(4);
    filter_list_box.set_margin_end(4);

    let filter_entries: Rc<RefCell<Vec<Entry>>> = Rc::new(RefCell::new(Vec::new()));

    let add_filter_entry = {
        let fe = filter_entries.clone();
        let flb = filter_list_box.clone();
        move |text: &str| {
            let row = GtkBox::new(Orientation::Horizontal, 4);
            let entry = Entry::new();
            entry.set_text(text);
            entry.set_hexpand(true);
            let remove_btn = Button::from_icon_name("list-remove-symbolic");
            remove_btn.set_tooltip_text(Some("Remove"));
            row.append(&entry);
            row.append(&remove_btn);
            flb.append(&row);
            fe.borrow_mut().push(entry.clone());

            let fe2 = fe.clone();
            let row_ref = row.clone();
            let flb2 = flb.clone();
            remove_btn.connect_clicked(move |_| {
                fe2.borrow_mut().retain(|e| e != &entry);
                flb2.remove(&row_ref);
            });
        }
    };

    for f in &s.dir_filters {
        add_filter_entry(f);
    }
    drop(s);

    let add_btn = Button::from_icon_name("list-add-symbolic");
    add_btn.set_tooltip_text(Some("Add filter"));
    add_btn.set_halign(gtk4::Align::Start);
    add_btn.set_margin_start(4);
    add_btn.set_margin_bottom(4);

    let filter_inner = GtkBox::new(Orientation::Vertical, 0);
    filter_inner.append(&filter_list_box);
    filter_inner.append(&add_btn);

    let filter_frame = gtk4::Frame::new(None);
    filter_frame.set_margin_start(12);
    filter_frame.set_margin_end(12);
    filter_frame.set_margin_top(4);
    filter_frame.set_child(Some(&filter_inner));
    content.append(&filter_frame);

    {
        let afe = add_filter_entry.clone();
        add_btn.connect_clicked(move |_| afe(""));
    }

    // ── Live-apply: shared closure to read controls, save, and apply ──
    let apply: Rc<dyn Fn()> = {
        let st = settings.clone();
        let p = parent.clone();
        let fb = font_btn.clone();
        let sd = scheme_dropdown.clone();
        let ss = scheme_strings.clone();
        let lns = line_num_switch.clone();
        let hls = highlight_switch.clone();
        let wd = wrap_dropdown.clone();
        let ts = tab_spin.clone();
        let fe = filter_entries.clone();
        let hs = hidden_switch.clone();
        let wns = wrap_nav_switch.clone();
        let iss = insert_spaces_switch.clone();
        let sws = show_ws_switch.clone();
        let ibs = ignore_blanks_switch.clone();
        let iwss = ignore_ws_switch.clone();
        Rc::new(move || {
            let mut s = st.borrow_mut();
            if let Some(fd) = fb.font_desc() {
                s.font = fd.to_string();
            }
            let idx = sd.selected() as usize;
            if idx < ss.len() {
                s.style_scheme.clone_from(&ss[idx]);
            }
            s.show_line_numbers = lns.is_active();
            s.highlight_current_line = hls.is_active();
            s.wrap_mode = match wd.selected() {
                1 => "word".into(),
                2 => "char".into(),
                _ => "none".into(),
            };
            s.tab_width = ts.value() as u32;
            s.insert_spaces = iss.is_active();
            s.show_whitespace = sws.is_active();
            s.hide_hidden_files = hs.is_active();
            s.wrap_around_navigation = wns.is_active();
            s.ignore_blank_lines = ibs.is_active();
            s.ignore_whitespace = iwss.is_active();
            s.dir_filters = fe
                .borrow()
                .iter()
                .map(|e| e.text().to_string())
                .filter(|t| !t.is_empty())
                .collect();
            s.save();
            apply_settings_to_views(p.upcast_ref(), &s);
        })
    };

    // Connect change signals for live-apply
    {
        let a = apply.clone();
        font_btn.connect_font_desc_notify(move |_| a());
    }
    {
        let a = apply.clone();
        scheme_dropdown.connect_selected_notify(move |_| a());
    }
    {
        let a = apply.clone();
        line_num_switch.connect_active_notify(move |_| a());
    }
    {
        let a = apply.clone();
        highlight_switch.connect_active_notify(move |_| a());
    }
    {
        let a = apply.clone();
        wrap_dropdown.connect_selected_notify(move |_| a());
    }
    {
        let a = apply.clone();
        tab_spin.connect_value_changed(move |_| a());
    }
    {
        let a = apply.clone();
        hidden_switch.connect_active_notify(move |_| a());
    }
    {
        let a = apply.clone();
        wrap_nav_switch.connect_active_notify(move |_| a());
    }
    {
        let a = apply.clone();
        insert_spaces_switch.connect_active_notify(move |_| a());
    }
    {
        let a = apply.clone();
        show_ws_switch.connect_active_notify(move |_| a());
    }
    {
        let a = apply.clone();
        ignore_blanks_switch.connect_active_notify(move |_| a());
    }
    {
        let a = apply.clone();
        ignore_ws_switch.connect_active_notify(move |_| a());
    }

    // Also save filters on close (they don't need live-apply)
    {
        let a = apply.clone();
        win.connect_close_request(move |_| {
            a();
            gtk4::glib::Propagation::Proceed
        });
    }

    // Bottom buttons row
    let bottom_row = GtkBox::new(Orientation::Horizontal, 8);
    bottom_row.set_margin_top(12);
    bottom_row.set_margin_start(12);
    bottom_row.set_margin_end(12);
    bottom_row.set_margin_bottom(8);

    let shortcuts_btn = Button::with_label("Keyboard Shortcuts");
    {
        let w = win.clone();
        shortcuts_btn.connect_clicked(move |_| show_shortcuts_dialog(&w));
    }
    bottom_row.append(&shortcuts_btn);

    let close_btn = Button::with_label("Close");
    close_btn.set_halign(gtk4::Align::End);
    close_btn.set_hexpand(true);
    {
        let w = win.clone();
        close_btn.connect_clicked(move |_| w.close());
    }
    bottom_row.append(&close_btn);

    content.append(&bottom_row);

    let scroll = ScrolledWindow::builder()
        .hscrollbar_policy(PolicyType::Never)
        .child(&content)
        .build();
    win.set_child(Some(&scroll));
    win.present();
}

fn make_shortcut_row(shortcut: &str, description: &str) -> GtkBox {
    let row = GtkBox::new(Orientation::Horizontal, 12);
    row.set_margin_start(12);
    row.set_margin_end(12);
    row.set_margin_top(2);
    row.set_margin_bottom(2);
    let key_label = Label::new(Some(shortcut));
    key_label.set_halign(gtk4::Align::End);
    key_label.set_width_chars(20);
    key_label.add_css_class("monospace");
    let desc_label = Label::new(Some(description));
    desc_label.set_halign(gtk4::Align::Start);
    desc_label.set_hexpand(true);
    row.append(&key_label);
    row.append(&desc_label);
    row
}

fn add_shortcuts_section(content: &GtkBox, title: &str, shortcuts: &[(String, &str)]) {
    let header = Label::new(Some(title));
    header.set_halign(gtk4::Align::Start);
    header.set_margin_start(12);
    header.set_margin_top(12);
    header.set_margin_bottom(4);
    header.add_css_class("heading");
    content.append(&header);

    for (key, desc) in shortcuts {
        content.append(&make_shortcut_row(key, desc));
    }
}

fn show_shortcuts_dialog(parent: &gtk4::Window) {
    let win = gtk4::Window::builder()
        .title("Keyboard Shortcuts")
        .transient_for(parent)
        .modal(true)
        .default_width(420)
        .default_height(500)
        .build();

    let w = win.clone();
    let key_ctl = EventControllerKey::new();
    key_ctl.connect_key_pressed(move |_, key, _, _| {
        if key == gtk4::gdk::Key::Escape {
            w.close();
            return gtk4::glib::Propagation::Stop;
        }
        gtk4::glib::Propagation::Proceed
    });
    win.add_controller(key_ctl);

    let m = primary_key_name();
    let content = GtkBox::new(Orientation::Vertical, 0);
    content.set_margin_top(8);
    content.set_margin_bottom(8);

    add_shortcuts_section(
        &content,
        "Navigation",
        &[
            (format!("{m}+E / Alt+Up"), "Previous change"),
            (format!("{m}+D / Alt+Down"), "Next change"),
            (format!("{m}+J"), "Previous conflict (merge)"),
            (format!("{m}+K"), "Next conflict (merge)"),
        ],
    );
    add_shortcuts_section(
        &content,
        "Editing",
        &[
            (format!("{m}+Z"), "Undo"),
            (format!("{m}+Shift+Z"), "Redo"),
            (format!("{m}+S"), "Save"),
            ("Alt+Left".into(), "Copy left"),
            ("Alt+Right".into(), "Copy right"),
        ],
    );
    add_shortcuts_section(
        &content,
        "Search",
        &[
            (format!("{m}+F"), "Find"),
            (format!("{m}+H"), "Find & Replace"),
            ("F3".into(), "Find next"),
            ("Shift+F3".into(), "Find previous"),
            (format!("{m}+L"), "Go to line"),
        ],
    );
    add_shortcuts_section(
        &content,
        "Other",
        &[
            (format!("{m}+Shift+P"), "Export patch"),
            (format!("{m}+,"), "Preferences"),
            (format!("{m}+W"), "Close tab"),
            (format!("{m}+Q"), "Quit"),
        ],
    );

    let scroll = ScrolledWindow::builder()
        .hscrollbar_policy(PolicyType::Never)
        .child(&content)
        .build();
    win.set_child(Some(&scroll));
    win.present();
}
