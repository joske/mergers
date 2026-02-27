#[allow(clippy::wildcard_imports)]
use super::*;

// ─── Shared diff view construction ─────────────────────────────────────────

pub(super) struct DiffViewResult {
    pub(super) widget: GtkBox,
    pub(super) left_buf: TextBuffer,
    pub(super) right_buf: TextBuffer,
    pub(super) left_save: Button,
    pub(super) right_save: Button,
    pub(super) action_group: gio::SimpleActionGroup,
}

/// Build a complete diff view widget for two files.
/// Returns the top-level widget (toolbar + diff panes) and associated state.
pub(super) fn build_diff_view(
    left_path: &Path,
    right_path: &Path,
    labels: &[String],
    settings: &Rc<RefCell<Settings>>,
) -> DiffViewResult {
    let s = settings.borrow();
    let (left_content, left_binary) = read_file_content(left_path);
    let (right_content, right_binary) = read_file_content(right_path);
    let any_binary = left_binary || right_binary;

    let left_buf = create_source_buffer(left_path, &s);
    let right_buf = create_source_buffer(right_path, &s);
    left_buf.set_text(&left_content);
    right_buf.set_text(&right_content);

    let identical = !any_binary && left_content == right_content;
    let chunks = Rc::new(RefCell::new(Vec::new()));

    let binary_msg = if any_binary {
        Some("Binary file — cannot display diff")
    } else {
        None
    };

    let left_label = labels.first().map(String::as_str);
    let right_label = labels.get(1).map(String::as_str);
    let left_pane = make_diff_pane(&left_buf, left_path, binary_msg, left_label, &s);
    let right_pane = make_diff_pane(&right_buf, right_path, binary_msg, right_label, &s);
    drop(s);

    // "Files are identical" info bar — dynamically shown/hidden on re-diff
    let identical_bars: Vec<GtkBox> = if any_binary {
        Vec::new()
    } else {
        [&left_pane, &right_pane]
            .iter()
            .map(|pane| {
                let bar = make_info_bar("Files are identical");
                bar.set_visible(identical);
                pane.container
                    .insert_child_after(&bar, pane.container.first_child().as_ref());
                bar
            })
            .collect()
    };

    if any_binary {
        left_pane.text_view.set_editable(false);
        right_pane.text_view.set_editable(false);
    }

    // Track which text view was last focused (for undo/redo, find, go-to-line)
    let active_view: Rc<RefCell<TextView>> = Rc::new(RefCell::new(left_pane.text_view.clone()));
    {
        let av = active_view.clone();
        let tv = left_pane.text_view.clone();
        let fc = EventControllerFocus::new();
        fc.connect_enter(move |_| {
            *av.borrow_mut() = tv.clone();
        });
        left_pane.text_view.add_controller(fc);
    }
    {
        let av = active_view.clone();
        let tv = right_pane.text_view.clone();
        let fc = EventControllerFocus::new();
        fc.connect_enter(move |_| {
            *av.borrow_mut() = tv.clone();
        });
        right_pane.text_view.add_controller(fc);
    }

    // Gutter (link map between left and right)
    let gutter = DrawingArea::new();
    gutter.set_content_width(48);
    gutter.set_vexpand(true);

    // Draw connecting bands + arrows
    {
        let ltv = left_pane.text_view.clone();
        let rtv = right_pane.text_view.clone();
        let lb = left_buf.clone();
        let rb = right_buf.clone();
        let ls = left_pane.scroll.clone();
        let rs = right_pane.scroll.clone();
        let ch = chunks.clone();
        gutter.set_draw_func(move |area, cr, width, _height| {
            draw_gutter(
                cr,
                width as f64,
                &ltv,
                &rtv,
                &lb,
                &rb,
                &ls,
                &rs,
                area,
                &ch.borrow(),
            );
        });
    }

    // Click handler for arrows
    {
        let gesture = GestureClick::new();
        let ltv = left_pane.text_view.clone();
        let rtv = right_pane.text_view.clone();
        let lb = left_buf.clone();
        let rb = right_buf.clone();
        let ls = left_pane.scroll.clone();
        let rs = right_pane.scroll.clone();
        let ch = chunks.clone();
        let g = gutter.clone();
        gesture.connect_pressed(move |_, _, x, y| {
            handle_gutter_click(
                x,
                y,
                g.width() as f64,
                &ltv,
                &rtv,
                &lb,
                &rb,
                &ls,
                &rs,
                &g,
                &ch,
            );
        });
        gutter.add_controller(gesture);
    }

    // Right-click context menu for gutter
    {
        let gutter_pending: Rc<Cell<Option<usize>>> = Rc::new(Cell::new(None));
        let gutter_ctx = gio::SimpleActionGroup::new();
        {
            let action = gio::SimpleAction::new("copy-left-right", None);
            let pc = gutter_pending.clone();
            let ch = chunks.clone();
            let lb = left_buf.clone();
            let rb = right_buf.clone();
            action.connect_activate(move |_, _| {
                if let Some(idx) = pc.get() {
                    let snapshot = ch.borrow();
                    if let Some(c) = snapshot.get(idx) {
                        copy_chunk(&lb, c.start_a, c.end_a, &rb, c.start_b, c.end_b);
                    }
                }
            });
            gutter_ctx.add_action(&action);
        }
        {
            let action = gio::SimpleAction::new("copy-right-left", None);
            let pc = gutter_pending.clone();
            let ch = chunks.clone();
            let lb = left_buf.clone();
            let rb = right_buf.clone();
            action.connect_activate(move |_, _| {
                if let Some(idx) = pc.get() {
                    let snapshot = ch.borrow();
                    if let Some(c) = snapshot.get(idx) {
                        copy_chunk(&rb, c.start_b, c.end_b, &lb, c.start_a, c.end_a);
                    }
                }
            });
            gutter_ctx.add_action(&action);
        }
        gutter.insert_action_group("gutter", Some(&gutter_ctx));

        let gutter_menu = gio::Menu::new();
        gutter_menu.append(
            Some("Copy Left \u{2192} Right"),
            Some("gutter.copy-left-right"),
        );
        gutter_menu.append(
            Some("Copy Right \u{2192} Left"),
            Some("gutter.copy-right-left"),
        );
        let gutter_popover = PopoverMenu::from_model(Some(&gutter_menu));
        gutter_popover.set_parent(&gutter);
        gutter_popover.set_has_arrow(false);

        let gesture = GestureClick::new();
        gesture.set_button(3);
        let ltv = left_pane.text_view.clone();
        let rtv = right_pane.text_view.clone();
        let lb = left_buf.clone();
        let rb = right_buf.clone();
        let ls = left_pane.scroll.clone();
        let rs = right_pane.scroll.clone();
        let ch = chunks.clone();
        let g = gutter.clone();
        let pc = gutter_pending.clone();
        let pop = gutter_popover;
        gesture.connect_pressed(move |_, _, x, y| {
            let snapshot = ch.borrow();
            for (idx, chunk) in snapshot.iter().enumerate() {
                if chunk.tag == DiffTag::Equal {
                    continue;
                }
                let lt = line_to_gutter_y(&ltv, &lb, chunk.start_a, &ls, &g);
                let lb_y = line_to_gutter_y(&ltv, &lb, chunk.end_a, &ls, &g);
                let rt = line_to_gutter_y(&rtv, &rb, chunk.start_b, &rs, &g);
                let rb_y = line_to_gutter_y(&rtv, &rb, chunk.end_b, &rs, &g);
                let top = lt.min(rt) - 6.0;
                let bottom = lb_y.max(rb_y) + 6.0;
                if y >= top && y <= bottom {
                    pc.set(Some(idx));
                    pop.set_pointing_to(Some(&gtk4::gdk::Rectangle::new(x as i32, y as i32, 1, 1)));
                    pop.popup();
                    return;
                }
            }
        });
        gutter.add_controller(gesture);
    }

    // Text filter state (created early so connect_changed can use it)
    let ignore_blanks: Rc<Cell<bool>> = Rc::new(Cell::new(false));
    let ignore_whitespace: Rc<Cell<bool>> = Rc::new(Cell::new(false));

    // Scroll synchronization
    setup_scroll_sync(&left_pane.scroll, &right_pane.scroll, &gutter);

    // ── Toolbar with chunk navigation ───────────────────────────
    let current_chunk: Rc<Cell<Option<usize>>> = Rc::new(Cell::new(None));

    // ── Filler overlay drawing ──────────────────────────────────
    {
        let ltv = left_pane.text_view.clone();
        let ls = left_pane.scroll.clone();
        let ch = chunks.clone();
        let cur = current_chunk.clone();
        left_pane
            .filler_overlay
            .set_draw_func(move |_area, cr, w, _h| {
                let w = w as f64;
                draw_chunk_backgrounds(cr, w, &ltv, &ls, &ch.borrow(), Side::A, cur.get());
                draw_fillers(cr, w, &ltv, &ls, &ch.borrow(), true);
            });
    }
    {
        let rtv = right_pane.text_view.clone();
        let rs = right_pane.scroll.clone();
        let ch = chunks.clone();
        let cur = current_chunk.clone();
        right_pane
            .filler_overlay
            .set_draw_func(move |_area, cr, w, _h| {
                let w = w as f64;
                draw_chunk_backgrounds(cr, w, &rtv, &rs, &ch.borrow(), Side::B, cur.get());
                draw_fillers(cr, w, &rtv, &rs, &ch.borrow(), false);
            });
    }
    // Redraw filler overlays on scroll
    {
        let lf = left_pane.filler_overlay.clone();
        let rf = right_pane.filler_overlay.clone();
        left_pane
            .scroll
            .vadjustment()
            .connect_value_changed(move |_| {
                lf.queue_draw();
                rf.queue_draw();
            });
    }
    {
        let lf = left_pane.filler_overlay.clone();
        let rf = right_pane.filler_overlay.clone();
        right_pane
            .scroll
            .vadjustment()
            .connect_value_changed(move |_| {
                lf.queue_draw();
                rf.queue_draw();
            });
    }

    let chunk_label = Label::new(None);
    chunk_label.add_css_class("chunk-label");
    update_chunk_label(&chunk_label, &chunks.borrow(), None);

    let prev_btn = Button::from_icon_name("go-up-symbolic");
    prev_btn.set_tooltip_text(Some("Previous change (Alt+Up / Ctrl+E)"));
    let next_btn = Button::from_icon_name("go-down-symbolic");
    next_btn.set_tooltip_text(Some("Next change (Alt+Down / Ctrl+D)"));

    let nav_box = GtkBox::new(Orientation::Horizontal, 0);
    nav_box.add_css_class("linked");
    nav_box.append(&prev_btn);
    nav_box.append(&next_btn);

    // Undo/Redo buttons
    let undo_btn = Button::from_icon_name("edit-undo-symbolic");
    undo_btn.set_tooltip_text(Some("Undo (Ctrl+Z)"));
    let redo_btn = Button::from_icon_name("edit-redo-symbolic");
    redo_btn.set_tooltip_text(Some("Redo (Ctrl+Shift+Z)"));
    let undo_redo_box = GtkBox::new(Orientation::Horizontal, 0);
    undo_redo_box.add_css_class("linked");
    undo_redo_box.append(&undo_btn);
    undo_redo_box.append(&redo_btn);

    {
        let av = active_view.clone();
        undo_btn.connect_clicked(move |_| {
            let buf = av.borrow().buffer();
            if buf.can_undo() {
                buf.undo();
            }
        });
    }
    {
        let av = active_view.clone();
        redo_btn.connect_clicked(move |_| {
            let buf = av.borrow().buffer();
            if buf.can_redo() {
                buf.redo();
            }
        });
    }

    // Go to line entry (hidden by default, shown by Ctrl+L)
    let goto_entry = Entry::new();
    goto_entry.set_placeholder_text(Some("Line #"));
    goto_entry.set_width_chars(8);
    goto_entry.add_css_class("goto-entry");
    goto_entry.set_visible(false);
    {
        let av = active_view.clone();
        let ls = left_pane.scroll.clone();
        let entry = goto_entry.clone();
        goto_entry.connect_activate(move |e| {
            if let Ok(line) = e.text().trim().parse::<usize>() {
                let tv = av.borrow().clone();
                let buf = tv.buffer();
                let target = line.saturating_sub(1); // 1-indexed to 0-indexed
                let scroll = scroll_for_view(&tv, &ls);
                scroll_to_line(&tv, &buf, target, &scroll);
                if let Some(iter) = buf.iter_at_line(target as i32) {
                    buf.place_cursor(&iter);
                }
            }
            e.set_visible(false);
            entry.set_text("");
        });
    }
    {
        let entry = goto_entry.clone();
        let key_ctl = EventControllerKey::new();
        key_ctl.connect_key_pressed(move |_, key, _, _| {
            if key == gtk4::gdk::Key::Escape {
                entry.set_visible(false);
                entry.set_text("");
                return gtk4::glib::Propagation::Stop;
            }
            gtk4::glib::Propagation::Proceed
        });
        goto_entry.add_controller(key_ctl);
    }

    let toolbar = GtkBox::new(Orientation::Horizontal, 8);
    toolbar.set_margin_start(6);
    toolbar.set_margin_end(6);
    toolbar.set_margin_top(4);
    toolbar.set_margin_bottom(4);
    toolbar.append(&undo_redo_box);
    let swap_btn = Button::from_icon_name("object-flip-horizontal-symbolic");
    swap_btn.set_tooltip_text(Some("Swap panes"));

    // Text filter toggles
    let blank_toggle = ToggleButton::with_label("Blanks");
    blank_toggle.set_tooltip_text(Some("Ignore blank lines"));
    let ws_toggle = ToggleButton::with_label("Spaces");
    ws_toggle.set_tooltip_text(Some("Ignore whitespace differences"));

    let filter_box = GtkBox::new(Orientation::Horizontal, 0);
    filter_box.add_css_class("linked");
    filter_box.append(&blank_toggle);
    filter_box.append(&ws_toggle);

    let patch_btn = Button::from_icon_name("document-save-as-symbolic");
    patch_btn.set_tooltip_text(Some("Export patch (Ctrl+Shift+P)"));
    if any_binary {
        patch_btn.set_sensitive(false);
    }

    toolbar.append(&nav_box);
    toolbar.append(&chunk_label);
    toolbar.append(&goto_entry);
    toolbar.append(&filter_box);
    toolbar.append(&patch_btn);
    toolbar.append(&swap_btn);
    let prefs_btn = Button::from_icon_name("preferences-system-symbolic");
    prefs_btn.set_tooltip_text(Some("Preferences (Ctrl+,)"));
    prefs_btn.set_action_name(Some("win.prefs"));
    toolbar.append(&prefs_btn);

    // Swap panes: swap buffer text + labels + save paths, re-diff happens via connect_changed
    {
        let lb = left_buf.clone();
        let rb = right_buf.clone();
        let ll = left_pane.path_label.clone();
        let rl = right_pane.path_label.clone();
        let lsp = left_pane.save_path.clone();
        let rsp = right_pane.save_path.clone();
        swap_btn.connect_clicked(move |_| {
            let lt = lb.text(&lb.start_iter(), &lb.end_iter(), false).to_string();
            let rt = rb.text(&rb.start_iter(), &rb.end_iter(), false).to_string();
            lb.set_text(&rt);
            rb.set_text(&lt);
            let ll_text = ll.text().to_string();
            let rl_text = rl.text().to_string();
            ll.set_text(&rl_text);
            rl.set_text(&ll_text);
            std::mem::swap(&mut *lsp.borrow_mut(), &mut *rsp.borrow_mut());
        });
    }

    // Prev chunk
    {
        let ch = chunks.clone();
        let cur = current_chunk.clone();
        let ltv = left_pane.text_view.clone();
        let rtv = right_pane.text_view.clone();
        let lb = left_buf.clone();
        let rb = right_buf.clone();
        let ls = left_pane.scroll.clone();
        let rs = right_pane.scroll.clone();
        let lbl = chunk_label.clone();
        let lf = left_pane.filler_overlay.clone();
        let rf = right_pane.filler_overlay.clone();
        let av = active_view.clone();
        prev_btn.connect_clicked(move |_| {
            let cl = cursor_line_from_view(&av.borrow());
            navigate_chunk(&ch.borrow(), &cur, -1, &ltv, &lb, &ls, &rtv, &rb, &rs, cl);
            update_chunk_label(&lbl, &ch.borrow(), cur.get());
            lf.queue_draw();
            rf.queue_draw();
        });
    }

    // Next chunk
    {
        let ch = chunks.clone();
        let cur = current_chunk.clone();
        let ltv = left_pane.text_view.clone();
        let rtv = right_pane.text_view.clone();
        let lb = left_buf.clone();
        let rb = right_buf.clone();
        let ls = left_pane.scroll.clone();
        let rs = right_pane.scroll.clone();
        let lbl = chunk_label.clone();
        let lf = left_pane.filler_overlay.clone();
        let rf = right_pane.filler_overlay.clone();
        let av = active_view.clone();
        next_btn.connect_clicked(move |_| {
            let cl = cursor_line_from_view(&av.borrow());
            navigate_chunk(&ch.borrow(), &cur, 1, &ltv, &lb, &ls, &rtv, &rb, &rs, cl);
            update_chunk_label(&lbl, &ch.borrow(), cur.get());
            lf.queue_draw();
            rf.queue_draw();
        });
    }

    // ── Chunk maps (overview strips) ─────────────────────────────
    let left_chunk_map = DrawingArea::new();
    left_chunk_map.set_content_width(12);
    left_chunk_map.set_vexpand(true);
    {
        let lb = left_buf.clone();
        let ls = left_pane.scroll.clone();
        let ch = chunks.clone();
        left_chunk_map.set_draw_func(move |_area, cr, _w, h| {
            draw_chunk_map(cr, h as f64, lb.line_count(), &ls, &ch.borrow(), true);
        });
    }
    // Click to jump
    {
        let gesture = GestureClick::new();
        let ls = left_pane.scroll.clone();
        let lm = left_chunk_map.clone();
        gesture.connect_pressed(move |_, _, _x, y| {
            let h = lm.height() as f64;
            if h > 0.0 {
                let adj = ls.vadjustment();
                let target = (y / h) * adj.upper() - adj.page_size() / 2.0;
                adj.set_value(target.max(0.0));
            }
        });
        left_chunk_map.add_controller(gesture);
    }

    let right_chunk_map = DrawingArea::new();
    right_chunk_map.set_content_width(12);
    right_chunk_map.set_vexpand(true);
    {
        let rb = right_buf.clone();
        let rs = right_pane.scroll.clone();
        let ch = chunks.clone();
        right_chunk_map.set_draw_func(move |_area, cr, _w, h| {
            draw_chunk_map(cr, h as f64, rb.line_count(), &rs, &ch.borrow(), false);
        });
    }
    {
        let gesture = GestureClick::new();
        let rs = right_pane.scroll.clone();
        let rm = right_chunk_map.clone();
        gesture.connect_pressed(move |_, _, _x, y| {
            let h = rm.height() as f64;
            if h > 0.0 {
                let adj = rs.vadjustment();
                let target = (y / h) * adj.upper() - adj.page_size() / 2.0;
                adj.set_value(target.max(0.0));
            }
        });
        right_chunk_map.add_controller(gesture);
    }

    // Redraw chunk maps on scroll and buffer changes
    {
        let lcm = left_chunk_map.clone();
        let rcm = right_chunk_map.clone();
        left_pane
            .scroll
            .vadjustment()
            .connect_value_changed(move |_| {
                lcm.queue_draw();
                rcm.queue_draw();
            });
    }
    {
        let lcm = left_chunk_map.clone();
        let rcm = right_chunk_map.clone();
        right_pane
            .scroll
            .vadjustment()
            .connect_value_changed(move |_| {
                lcm.queue_draw();
                rcm.queue_draw();
            });
    }

    // Re-diff on any buffer change, and update all visuals when diff completes
    {
        let make_on_complete = {
            let g = gutter.clone();
            let lcm = left_chunk_map.clone();
            let rcm = right_chunk_map.clone();
            let lbl = chunk_label.clone();
            let ch = chunks.clone();
            let cur = current_chunk.clone();
            let lf = left_pane.filler_overlay.clone();
            let rf = right_pane.filler_overlay.clone();
            let ibars = identical_bars.clone();
            move || {
                let g = g.clone();
                let lcm = lcm.clone();
                let rcm = rcm.clone();
                let lbl = lbl.clone();
                let ch = ch.clone();
                let cur = cur.clone();
                let lf = lf.clone();
                let rf = rf.clone();
                let ibars = ibars.clone();
                move || {
                    g.queue_draw();
                    lcm.queue_draw();
                    rcm.queue_draw();
                    lf.queue_draw();
                    rf.queue_draw();
                    cur.set(None);
                    update_chunk_label(&lbl, &ch.borrow(), None);
                    let is_identical = ch.borrow().is_empty();
                    for bar in &ibars {
                        bar.set_visible(is_identical);
                    }
                }
            }
        };

        let pending = Rc::new(Cell::new(false));
        let connect_refresh = |buf: &TextBuffer| {
            let lb = left_buf.clone();
            let rb = right_buf.clone();
            let ch = chunks.clone();
            let p = pending.clone();
            let ib = ignore_blanks.clone();
            let iw = ignore_whitespace.clone();
            let make_cb = make_on_complete.clone();
            buf.connect_changed(move |_| {
                if !p.get() {
                    p.set(true);
                    let lb = lb.clone();
                    let rb = rb.clone();
                    let ch = ch.clone();
                    let p = p.clone();
                    let ib = ib.clone();
                    let iw = iw.clone();
                    let cb = make_cb();
                    gtk4::glib::idle_add_local_once(move || {
                        refresh_diff(&lb, &rb, &ch, cb, ib.get(), iw.get(), &p);
                    });
                }
            });
        };
        connect_refresh(&left_buf);
        connect_refresh(&right_buf);

        // Initial async diff (must be after chunk_label + chunk_maps exist)
        if !identical && !any_binary {
            let lb = left_buf.clone();
            let rb = right_buf.clone();
            let ch = chunks.clone();
            let on_complete = make_on_complete();
            pending.set(true);
            let p = pending.clone();
            let ib = ignore_blanks.get();
            let iw = ignore_whitespace.get();
            gtk4::glib::spawn_future_local(async move {
                let (lt_cmp, lt_map) = filter_for_diff(&left_content, iw, ib);
                let (rt_cmp, rt_map) = filter_for_diff(&right_content, iw, ib);
                let lt_total = left_content.lines().count();
                let rt_total = right_content.lines().count();
                let new_chunks = if lt_cmp == rt_cmp {
                    Vec::new()
                } else {
                    let raw = gio::spawn_blocking(move || myers::diff_lines(&lt_cmp, &rt_cmp))
                        .await
                        .unwrap_or_default();
                    remap_chunks(raw, &lt_map, lt_total, &rt_map, rt_total)
                };
                apply_diff_tags(&lb, &rb, &new_chunks);
                *ch.borrow_mut() = new_chunks;
                on_complete();
                p.set(false);
            });
        }

        // Toggle handlers for filter buttons
        {
            let ib = ignore_blanks.clone();
            let iw = ignore_whitespace.clone();
            let lb = left_buf.clone();
            let rb = right_buf.clone();
            let ch = chunks.clone();
            let make_cb = make_on_complete.clone();
            let dummy = Rc::new(Cell::new(true));
            blank_toggle.connect_toggled(move |btn| {
                ib.set(btn.is_active());
                dummy.set(true);
                refresh_diff(&lb, &rb, &ch, make_cb(), ib.get(), iw.get(), &dummy);
            });
        }
        {
            let ib = ignore_blanks.clone();
            let iw = ignore_whitespace.clone();
            let lb = left_buf.clone();
            let rb = right_buf.clone();
            let ch = chunks.clone();
            let dummy = Rc::new(Cell::new(true));
            ws_toggle.connect_toggled(move |btn| {
                iw.set(btn.is_active());
                dummy.set(true);
                refresh_diff(
                    &lb,
                    &rb,
                    &ch,
                    make_on_complete(),
                    ib.get(),
                    iw.get(),
                    &dummy,
                );
            });
        }
    }

    // ── Find bar ──────────────────────────────────────────────────
    let find_entry = Entry::new();
    find_entry.set_placeholder_text(Some("Find (Ctrl+F)"));
    find_entry.set_hexpand(true);

    let replace_entry = Entry::new();
    replace_entry.set_placeholder_text(Some("Replace"));
    replace_entry.set_hexpand(true);

    let find_prev_btn = Button::from_icon_name("go-up-symbolic");
    find_prev_btn.set_tooltip_text(Some("Previous match (Shift+F3)"));
    let find_next_btn = Button::from_icon_name("go-down-symbolic");
    find_next_btn.set_tooltip_text(Some("Next match (F3)"));
    let match_label = Label::new(None);
    match_label.add_css_class("chunk-label");
    let find_close_btn = Button::from_icon_name("window-close-symbolic");
    find_close_btn.set_has_frame(false);
    find_close_btn.set_tooltip_text(Some("Close (Escape)"));

    let replace_btn = Button::with_label("Replace");
    let replace_all_btn = Button::with_label("All");
    let replace_row = GtkBox::new(Orientation::Horizontal, 4);
    replace_row.set_margin_start(6);
    replace_row.set_margin_end(6);
    replace_row.append(&replace_entry);
    replace_row.append(&replace_btn);
    replace_row.append(&replace_all_btn);
    replace_row.set_visible(false);

    let find_nav = GtkBox::new(Orientation::Horizontal, 0);
    find_nav.add_css_class("linked");
    find_nav.append(&find_prev_btn);
    find_nav.append(&find_next_btn);

    let find_row = GtkBox::new(Orientation::Horizontal, 4);
    find_row.set_margin_start(6);
    find_row.set_margin_end(6);
    find_row.append(&find_entry);
    find_row.append(&find_nav);
    find_row.append(&match_label);
    find_row.append(&find_close_btn);

    let find_bar = GtkBox::new(Orientation::Vertical, 2);
    find_bar.add_css_class("find-bar");
    find_bar.append(&find_row);
    find_bar.append(&replace_row);

    let find_revealer = Revealer::new();
    find_revealer.set_child(Some(&find_bar));
    find_revealer.set_reveal_child(false);
    find_revealer.set_transition_type(gtk4::RevealerTransitionType::SlideUp);

    // Search logic: highlight matches when find entry text changes
    {
        let lb = left_buf.clone();
        let rb = right_buf.clone();
        let ml = match_label.clone();
        find_entry.connect_changed(move |e| {
            let needle = e.text().to_string();
            let lc = highlight_search_matches(&lb, &needle);
            let rc = highlight_search_matches(&rb, &needle);
            let total = lc + rc;
            if needle.is_empty() {
                ml.set_label("");
            } else if total == 0 {
                ml.set_label("No matches");
            } else {
                ml.set_label(&format!("{total} matches"));
            }
        });
    }

    // Find next
    {
        let av = active_view.clone();
        let fe = find_entry.clone();
        let ls = left_pane.scroll.clone();
        find_next_btn.connect_clicked(move |_| {
            let needle = fe.text().to_string();
            let tv = av.borrow().clone();
            let buf = tv.buffer();
            let cursor = buf.iter_at_mark(&buf.get_insert());
            if let Some((start, end)) = find_next_match(&buf, &needle, &cursor, true) {
                buf.select_range(&start, &end);
                let scroll = scroll_for_view(&tv, &ls);
                scroll_to_line(&tv, &buf, start.line() as usize, &scroll);
            }
        });
    }

    // Find prev
    {
        let av = active_view.clone();
        let fe = find_entry.clone();
        let ls = left_pane.scroll.clone();
        find_prev_btn.connect_clicked(move |_| {
            let needle = fe.text().to_string();
            let tv = av.borrow().clone();
            let buf = tv.buffer();
            let cursor = buf.iter_at_mark(&buf.get_insert());
            if let Some((start, end)) = find_next_match(&buf, &needle, &cursor, false) {
                buf.select_range(&start, &end);
                let scroll = scroll_for_view(&tv, &ls);
                scroll_to_line(&tv, &buf, start.line() as usize, &scroll);
            }
        });
    }

    // Find entry Enter = find next
    {
        let av = active_view.clone();
        let ls = left_pane.scroll.clone();
        find_entry.connect_activate(move |e| {
            let needle = e.text().to_string();
            let tv = av.borrow().clone();
            let buf = tv.buffer();
            let cursor = buf.iter_at_mark(&buf.get_insert());
            if let Some((start, end)) = find_next_match(&buf, &needle, &cursor, true) {
                buf.select_range(&start, &end);
                let scroll = scroll_for_view(&tv, &ls);
                scroll_to_line(&tv, &buf, start.line() as usize, &scroll);
            }
        });
    }

    // Replace
    {
        let av = active_view.clone();
        let find_e = find_entry.clone();
        let repl_e = replace_entry.clone();
        replace_btn.connect_clicked(move |_| {
            let tv = av.borrow().clone();
            let buf = tv.buffer();
            let (sel_start, sel_end) = buf.selection_bounds().unwrap_or_else(|| {
                let c = buf.iter_at_mark(&buf.get_insert());
                (c, c)
            });
            let selected = buf.text(&sel_start, &sel_end, false).to_string();
            let needle = find_e.text().to_string();
            if !needle.is_empty() && selected.to_lowercase() == needle.to_lowercase() {
                let replacement = repl_e.text().to_string();
                let mut s = sel_start;
                let mut e = sel_end;
                buf.delete(&mut s, &mut e);
                buf.insert(&mut s, &replacement);
            }
        });
    }

    // Replace all
    {
        let lb = left_buf.clone();
        let rb = right_buf.clone();
        let find_e = find_entry.clone();
        let repl_e = replace_entry.clone();
        replace_all_btn.connect_clicked(move |_| {
            let needle = find_e.text().to_string();
            let replacement = repl_e.text().to_string();
            if needle.is_empty() {
                return;
            }
            let needle_lower = needle.to_lowercase();
            for buf in [&lb, &rb] {
                let text = buf
                    .text(&buf.start_iter(), &buf.end_iter(), false)
                    .to_string();
                // Case-insensitive replace to match CASE_INSENSITIVE find
                let mut new_text = String::with_capacity(text.len());
                let mut remaining = text.as_str();
                while let Some(pos) = remaining.to_lowercase().find(&needle_lower) {
                    new_text.push_str(&remaining[..pos]);
                    new_text.push_str(&replacement);
                    remaining = &remaining[(pos + needle.len())..];
                }
                new_text.push_str(remaining);
                if new_text != text {
                    buf.set_text(&new_text);
                }
            }
        });
    }

    // Close find bar
    {
        let fr = find_revealer.clone();
        let lb = left_buf.clone();
        let rb = right_buf.clone();
        find_close_btn.connect_clicked(move |_| {
            fr.set_reveal_child(false);
            clear_search_tags(&lb);
            clear_search_tags(&rb);
        });
    }

    // Escape in find entry closes find bar
    {
        let fr = find_revealer.clone();
        let lb = left_buf.clone();
        let rb = right_buf.clone();
        let key_ctl = EventControllerKey::new();
        key_ctl.connect_key_pressed(move |_, key, _, _| {
            if key == gtk4::gdk::Key::Escape {
                fr.set_reveal_child(false);
                clear_search_tags(&lb);
                clear_search_tags(&rb);
                return gtk4::glib::Propagation::Stop;
            }
            gtk4::glib::Propagation::Proceed
        });
        find_entry.add_controller(key_ctl);
    }

    // Layout: toolbar + separator + [chunk_map | left pane | gutter | right pane | chunk_map] + find bar
    let diff_row = GtkBox::new(Orientation::Horizontal, 0);
    left_pane.container.set_hexpand(true);
    right_pane.container.set_hexpand(true);
    diff_row.append(&left_chunk_map);
    diff_row.append(&left_pane.container);
    diff_row.append(&gutter);
    diff_row.append(&right_pane.container);
    diff_row.append(&right_chunk_map);
    diff_row.set_vexpand(true);

    let widget = GtkBox::new(Orientation::Vertical, 0);
    widget.append(&toolbar);
    widget.append(&gtk4::Separator::new(Orientation::Horizontal));
    widget.append(&diff_row);
    widget.append(&find_revealer);

    // GAction group for keyboard shortcuts
    let action_group = gio::SimpleActionGroup::new();
    {
        let action = gio::SimpleAction::new("prev-chunk", None);
        let ch = chunks.clone();
        let cur = current_chunk.clone();
        let ltv = left_pane.text_view.clone();
        let rtv = right_pane.text_view.clone();
        let lb = left_buf.clone();
        let rb = right_buf.clone();
        let ls = left_pane.scroll.clone();
        let rs = right_pane.scroll.clone();
        let lbl = chunk_label.clone();
        let lf = left_pane.filler_overlay.clone();
        let rf = right_pane.filler_overlay.clone();
        let av = active_view.clone();
        action.connect_activate(move |_, _| {
            let cl = cursor_line_from_view(&av.borrow());
            navigate_chunk(&ch.borrow(), &cur, -1, &ltv, &lb, &ls, &rtv, &rb, &rs, cl);
            update_chunk_label(&lbl, &ch.borrow(), cur.get());
            lf.queue_draw();
            rf.queue_draw();
        });
        action_group.add_action(&action);
    }
    {
        let action = gio::SimpleAction::new("next-chunk", None);
        let ch = chunks.clone();
        let cur = current_chunk.clone();
        let ltv = left_pane.text_view.clone();
        let rtv = right_pane.text_view.clone();
        let lb = left_buf.clone();
        let rb = right_buf.clone();
        let ls = left_pane.scroll.clone();
        let rs = right_pane.scroll.clone();
        let lbl = chunk_label.clone();
        let lf = left_pane.filler_overlay.clone();
        let rf = right_pane.filler_overlay.clone();
        let av = active_view.clone();
        action.connect_activate(move |_, _| {
            let cl = cursor_line_from_view(&av.borrow());
            navigate_chunk(&ch.borrow(), &cur, 1, &ltv, &lb, &ls, &rtv, &rb, &rs, cl);
            update_chunk_label(&lbl, &ch.borrow(), cur.get());
            lf.queue_draw();
            rf.queue_draw();
        });
        action_group.add_action(&action);
    }
    // Find action (Ctrl+F)
    {
        let action = gio::SimpleAction::new("find", None);
        let fr = find_revealer.clone();
        let fe = find_entry.clone();
        let rr = replace_row.clone();
        action.connect_activate(move |_, _| {
            rr.set_visible(false);
            fr.set_reveal_child(true);
            fe.grab_focus();
        });
        action_group.add_action(&action);
    }
    // Find-replace action (Ctrl+H)
    {
        let action = gio::SimpleAction::new("find-replace", None);
        let fr = find_revealer.clone();
        let fe = find_entry.clone();
        let rr = replace_row.clone();
        action.connect_activate(move |_, _| {
            rr.set_visible(true);
            fr.set_reveal_child(true);
            fe.grab_focus();
        });
        action_group.add_action(&action);
    }
    // Find next (F3)
    {
        let action = gio::SimpleAction::new("find-next", None);
        let av = active_view.clone();
        let fe = find_entry.clone();
        let ls = left_pane.scroll.clone();
        action.connect_activate(move |_, _| {
            let needle = fe.text().to_string();
            let tv = av.borrow().clone();
            let buf = tv.buffer();
            let cursor = buf.iter_at_mark(&buf.get_insert());
            if let Some((start, end)) = find_next_match(&buf, &needle, &cursor, true) {
                buf.select_range(&start, &end);
                let scroll = scroll_for_view(&tv, &ls);
                scroll_to_line(&tv, &buf, start.line() as usize, &scroll);
            }
        });
        action_group.add_action(&action);
    }
    // Find prev (Shift+F3)
    {
        let action = gio::SimpleAction::new("find-prev", None);
        let av = active_view.clone();
        let fe = find_entry.clone();
        let ls = left_pane.scroll.clone();
        action.connect_activate(move |_, _| {
            let needle = fe.text().to_string();
            let tv = av.borrow().clone();
            let buf = tv.buffer();
            let cursor = buf.iter_at_mark(&buf.get_insert());
            if let Some((start, end)) = find_next_match(&buf, &needle, &cursor, false) {
                buf.select_range(&start, &end);
                let scroll = scroll_for_view(&tv, &ls);
                scroll_to_line(&tv, &buf, start.line() as usize, &scroll);
            }
        });
        action_group.add_action(&action);
    }
    // Go to line (Ctrl+L)
    {
        let action = gio::SimpleAction::new("go-to-line", None);
        let ge = goto_entry.clone();
        action.connect_activate(move |_, _| {
            ge.set_visible(true);
            ge.grab_focus();
        });
        action_group.add_action(&action);
    }

    // Export patch
    {
        let export_patch = {
            let lb = left_buf.clone();
            let rb = right_buf.clone();
            let ch = chunks.clone();
            let ll = left_pane.path_label.clone();
            let rl = right_pane.path_label.clone();
            let pb = patch_btn.clone();
            move || {
                let lt = lb.text(&lb.start_iter(), &lb.end_iter(), false).to_string();
                let rt = rb.text(&rb.start_iter(), &rb.end_iter(), false).to_string();
                let patch = generate_unified_diff(
                    &format!("a/{}", ll.text()),
                    &format!("b/{}", rl.text()),
                    &lt,
                    &rt,
                    &ch.borrow(),
                );
                let dialog = gtk4::FileDialog::builder()
                    .title("Export Patch")
                    .initial_name("diff.patch")
                    .build();
                let win = pb
                    .root()
                    .and_then(|r| r.downcast::<ApplicationWindow>().ok());
                let dialog_ref = dialog.clone();
                let pb2 = pb.clone();
                dialog_ref.save(win.as_ref(), gio::Cancellable::NONE, move |result| {
                    if let Ok(file) = result
                        && let Some(path) = file.path()
                        && let Err(e) = fs::write(&path, &patch)
                        && let Some(win) = pb2
                            .root()
                            .and_then(|r| r.downcast::<ApplicationWindow>().ok())
                    {
                        show_error_dialog(&win, &format!("Failed to export patch: {e}"));
                    }
                });
            }
        };

        let export = Rc::new(export_patch);
        {
            let e = export.clone();
            patch_btn.connect_clicked(move |_| e());
        }
        {
            let action = gio::SimpleAction::new("export-patch", None);
            let e = export.clone();
            action.connect_activate(move |_, _| e());
            action_group.add_action(&action);
        }
    }

    DiffViewResult {
        widget,
        left_buf,
        right_buf,
        left_save: left_pane.save_btn,
        right_save: right_pane.save_btn,
        action_group,
    }
}

// ─── Open file diff in new tab ─────────────────────────────────────────────

pub(super) fn open_file_diff(
    notebook: &Notebook,
    rel_path: &str,
    open_tabs: &Rc<RefCell<Vec<FileTab>>>,
    left_dir: &str,
    right_dir: &str,
    settings: &Rc<RefCell<Settings>>,
) {
    let left_path = Path::new(left_dir).join(rel_path);
    let right_path = Path::new(right_dir).join(rel_path);

    let dv = build_diff_view(&left_path, &right_path, &[], settings);
    dv.widget
        .insert_action_group("diff", Some(&dv.action_group));

    // Track tab
    let tab_id = NEXT_TAB_ID.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
    open_tabs.borrow_mut().push(FileTab {
        id: tab_id,
        rel_path: rel_path.to_string(),
        widget: dv.widget.clone(),
        left_path: left_path.display().to_string(),
        right_path: right_path.display().to_string(),
        left_buf: dv.left_buf,
        right_buf: dv.right_buf,
        left_save: dv.left_save,
        right_save: dv.right_save,
    });

    // Tab label
    let file_name = Path::new(rel_path).file_name().map_or_else(
        || rel_path.to_string(),
        |n| n.to_string_lossy().into_owned(),
    );
    let left_dir_name = Path::new(left_dir)
        .file_name()
        .map(|n| n.to_string_lossy().into_owned())
        .unwrap_or_default();
    let right_dir_name = Path::new(right_dir)
        .file_name()
        .map(|n| n.to_string_lossy().into_owned())
        .unwrap_or_default();
    let tab_title = format!("[{left_dir_name}] {file_name} — [{right_dir_name}] {file_name}");

    let tab_label_box = GtkBox::new(Orientation::Horizontal, 4);
    let label = Label::new(Some(&tab_title));
    let close_btn = Button::from_icon_name("window-close-symbolic");
    close_btn.set_has_frame(false);
    tab_label_box.append(&label);
    tab_label_box.append(&close_btn);

    let page_num = notebook.append_page(&dv.widget, Some(&tab_label_box));
    notebook.set_current_page(Some(page_num));

    let nb = notebook.clone();
    let w = dv.widget.clone();
    let tabs = open_tabs.clone();
    close_btn.connect_clicked(move |_| {
        if let Some(n) = nb.page_num(&w) {
            nb.remove_page(Some(n));
        }
        tabs.borrow_mut().retain(|t| t.id != tab_id);
    });
}
