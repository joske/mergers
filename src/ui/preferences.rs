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
            let buf: TextBuffer = sv.buffer();
            if let Ok(sbuf) = buf.downcast::<sourceview5::Buffer>() {
                let scheme_mgr = sourceview5::StyleSchemeManager::default();
                if let Some(scheme) = scheme_mgr.scheme(&settings.style_scheme) {
                    sbuf.set_style_scheme(Some(&scheme));
                }
            }
        }
        // Recurse into children
        let mut child = widget.first_child();
        while let Some(c) = child {
            walk(&c, settings);
            child = c.next_sibling();
        }
    }
    let w: gtk4::Widget = window.clone().upcast();
    walk(&w, settings);

    update_font_css(settings);
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
    if let Some(pos) = scheme_strings.iter().position(|id| id == &s.style_scheme) {
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

    // Also save filters on close (they don't need live-apply)
    {
        let a = apply.clone();
        win.connect_close_request(move |_| {
            a();
            gtk4::glib::Propagation::Proceed
        });
    }

    let scroll = ScrolledWindow::builder()
        .hscrollbar_policy(PolicyType::Never)
        .child(&content)
        .build();
    win.set_child(Some(&scroll));
    win.present();
}
