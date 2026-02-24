#[allow(clippy::wildcard_imports)]
use super::*;

// ─── 3-way merge view ───────────────────────────────────────────────────────

struct MergeViewResult {
    widget: GtkBox,
    left_buf: TextBuffer,
    middle_buf: TextBuffer,
    right_buf: TextBuffer,
    middle_save: Button,
    action_group: gio::SimpleActionGroup,
}

#[allow(clippy::too_many_arguments)]
fn refresh_merge_diffs(
    left_buf: &TextBuffer,
    middle_buf: &TextBuffer,
    right_buf: &TextBuffer,
    left_tv: &TextView,
    left_chunks: &Rc<RefCell<Vec<DiffChunk>>>,
    right_chunks: &Rc<RefCell<Vec<DiffChunk>>>,
    line_height_cell: &Rc<Cell<i32>>,
    on_complete: impl Fn() + 'static,
    ignore_blanks: bool,
    ignore_whitespace: bool,
    pending: &Rc<Cell<bool>>,
) {
    let lt = left_buf
        .text(&left_buf.start_iter(), &left_buf.end_iter(), false)
        .to_string();
    let mt = middle_buf
        .text(&middle_buf.start_iter(), &middle_buf.end_iter(), false)
        .to_string();
    let rt = right_buf
        .text(&right_buf.start_iter(), &right_buf.end_iter(), false)
        .to_string();

    remove_diff_tags(left_buf);
    remove_diff_tags(middle_buf);
    remove_diff_tags(right_buf);

    let lb = left_buf.clone();
    let mb = middle_buf.clone();
    let rb = right_buf.clone();
    let lch = left_chunks.clone();
    let rch = right_chunks.clone();
    let p = pending.clone();
    let lh = get_line_height(left_tv);
    line_height_cell.set(lh);

    gtk4::glib::spawn_future_local(async move {
        let (lt_cmp, lt_map) = filter_for_diff(&lt, ignore_whitespace, ignore_blanks);
        let (mt_cmp, mt_map) = filter_for_diff(&mt, ignore_whitespace, ignore_blanks);
        let (rt_cmp, rt_map) = filter_for_diff(&rt, ignore_whitespace, ignore_blanks);
        let lt_total = lt.lines().count();
        let mt_total = mt.lines().count();
        let rt_total = rt.lines().count();
        let left_identical = lt_cmp == mt_cmp;
        let right_identical = mt_cmp == rt_cmp;
        let (new_left, new_right) = gio::spawn_blocking(move || {
            let nl = if left_identical {
                Vec::new()
            } else {
                myers::diff_lines(&lt_cmp, &mt_cmp)
            };
            let nr = if right_identical {
                Vec::new()
            } else {
                myers::diff_lines(&mt_cmp, &rt_cmp)
            };
            (nl, nr)
        })
        .await
        .unwrap_or_default();

        let new_left = remap_chunks(new_left, &lt_map, lt_total, &mt_map, mt_total);
        let new_right = remap_chunks(new_right, &mt_map, mt_total, &rt_map, rt_total);

        apply_merge_tags(&lb, &mb, &rb, &new_left, &new_right);
        apply_merge_filler_tags(&lb, &mb, &rb, &new_left, &new_right, lh);

        *lch.borrow_mut() = new_left;
        *rch.borrow_mut() = new_right;
        on_complete();
        p.set(false);
    });
}

fn setup_scroll_sync_3way(
    left_scroll: &ScrolledWindow,
    middle_scroll: &ScrolledWindow,
    right_scroll: &ScrolledWindow,
    left_gutter: &DrawingArea,
    right_gutter: &DrawingArea,
) {
    let syncing = Rc::new(Cell::new(false));

    // Left → Middle, Right
    {
        let ms = middle_scroll.clone();
        let rs = right_scroll.clone();
        let s = syncing.clone();
        let lg = left_gutter.clone();
        let rg = right_gutter.clone();
        left_scroll.vadjustment().connect_value_changed(move |adj| {
            if !s.get() {
                s.set(true);
                sync_adjustment(&ms.vadjustment(), adj);
                sync_adjustment(&rs.vadjustment(), adj);
                lg.queue_draw();
                rg.queue_draw();
                s.set(false);
            }
        });
    }

    // Middle → Left, Right
    {
        let ls = left_scroll.clone();
        let rs = right_scroll.clone();
        let s = syncing.clone();
        let lg = left_gutter.clone();
        let rg = right_gutter.clone();
        middle_scroll
            .vadjustment()
            .connect_value_changed(move |adj| {
                if !s.get() {
                    s.set(true);
                    sync_adjustment(&ls.vadjustment(), adj);
                    sync_adjustment(&rs.vadjustment(), adj);
                    lg.queue_draw();
                    rg.queue_draw();
                    s.set(false);
                }
            });
    }

    // Right → Left, Middle
    {
        let ls = left_scroll.clone();
        let ms = middle_scroll.clone();
        let s = syncing.clone();
        let lg = left_gutter.clone();
        let rg = right_gutter.clone();
        right_scroll
            .vadjustment()
            .connect_value_changed(move |adj| {
                if !s.get() {
                    s.set(true);
                    sync_adjustment(&ls.vadjustment(), adj);
                    sync_adjustment(&ms.vadjustment(), adj);
                    lg.queue_draw();
                    rg.queue_draw();
                    s.set(false);
                }
            });
    }

    // Horizontal: Left → Middle, Right
    {
        let ms = middle_scroll.clone();
        let rs = right_scroll.clone();
        let s = syncing.clone();
        left_scroll.hadjustment().connect_value_changed(move |adj| {
            if !s.get() {
                s.set(true);
                ms.hadjustment().set_value(adj.value());
                rs.hadjustment().set_value(adj.value());
                s.set(false);
            }
        });
    }

    // Horizontal: Middle → Left, Right
    {
        let ls = left_scroll.clone();
        let rs = right_scroll.clone();
        let s = syncing.clone();
        middle_scroll
            .hadjustment()
            .connect_value_changed(move |adj| {
                if !s.get() {
                    s.set(true);
                    ls.hadjustment().set_value(adj.value());
                    rs.hadjustment().set_value(adj.value());
                    s.set(false);
                }
            });
    }

    // Horizontal: Right → Left, Middle
    {
        let ls = left_scroll.clone();
        let ms = middle_scroll.clone();
        let s = syncing.clone();
        right_scroll
            .hadjustment()
            .connect_value_changed(move |adj| {
                if !s.get() {
                    s.set(true);
                    ls.hadjustment().set_value(adj.value());
                    ms.hadjustment().set_value(adj.value());
                    s.set(false);
                }
            });
    }
}

/// Collect non-Equal chunks from both diffs, sorted by middle-file line position.
/// Returns (`chunk_index`, `is_right_diff`) pairs.
fn merge_change_indices(
    left_chunks: &[DiffChunk],
    right_chunks: &[DiffChunk],
) -> Vec<(usize, bool)> {
    let mut indices: Vec<(usize, bool, usize)> = Vec::new();
    for (i, c) in left_chunks.iter().enumerate() {
        if c.tag != DiffTag::Equal {
            // In left diff, middle lines = start_b
            indices.push((i, false, c.start_b));
        }
    }
    for (i, c) in right_chunks.iter().enumerate() {
        if c.tag != DiffTag::Equal {
            // In right diff, middle lines = start_a
            indices.push((i, true, c.start_a));
        }
    }
    indices.sort_by_key(|&(_, _, line)| line);
    indices
        .into_iter()
        .map(|(i, is_right, _)| (i, is_right))
        .collect()
}

/// Find line numbers of `<<<<<<<` conflict markers in a buffer.
fn find_conflict_markers(buf: &TextBuffer) -> Vec<usize> {
    let text = buf.text(&buf.start_iter(), &buf.end_iter(), false);
    text.lines()
        .enumerate()
        .filter_map(|(i, line)| {
            if line.starts_with("<<<<<<<") {
                Some(i)
            } else {
                None
            }
        })
        .collect()
}

fn build_merge_view(
    left_path: &Path,
    middle_path: &Path,
    right_path: &Path,
    output: Option<&Path>,
    labels: &[String],
    settings: &Rc<RefCell<Settings>>,
) -> MergeViewResult {
    let s = settings.borrow();
    let (left_content, left_binary) = read_file_content(left_path);
    // When --output is set, the middle pane shows the merged file (with conflict markers),
    // not the base. This matches git mergetool semantics: you edit $MERGED, not $BASE.
    let middle_display_path = output.unwrap_or(middle_path);
    let (middle_content, middle_binary) = read_file_content(middle_display_path);
    let (right_content, right_binary) = read_file_content(right_path);
    let any_binary = left_binary || middle_binary || right_binary;

    let left_buf = create_source_buffer(left_path, &s);
    let middle_buf = create_source_buffer(middle_display_path, &s);
    let right_buf = create_source_buffer(right_path, &s);
    left_buf.set_text(&left_content);
    middle_buf.set_text(&middle_content);
    right_buf.set_text(&right_content);

    let left_identical = !any_binary && left_content == middle_content;
    let right_identical = !any_binary && middle_content == right_content;

    let left_chunks = Rc::new(RefCell::new(Vec::new()));
    let right_chunks = Rc::new(RefCell::new(Vec::new()));

    let left_pane = make_diff_pane(
        &left_buf,
        left_path,
        None,
        labels.first().map(String::as_str),
        &s,
    );
    let middle_pane = make_diff_pane(
        &middle_buf,
        middle_display_path,
        None,
        labels.get(1).map(String::as_str),
        &s,
    );
    let right_pane = make_diff_pane(
        &right_buf,
        right_path,
        None,
        labels.get(2).map(String::as_str),
        &s,
    );
    drop(s);

    // Track which text view was last focused
    let active_view: Rc<RefCell<TextView>> = Rc::new(RefCell::new(middle_pane.text_view.clone()));
    for tv in [
        &left_pane.text_view,
        &middle_pane.text_view,
        &right_pane.text_view,
    ] {
        let av = active_view.clone();
        let t = tv.clone();
        let fc = EventControllerFocus::new();
        fc.connect_enter(move |_| {
            *av.borrow_mut() = t.clone();
        });
        tv.add_controller(fc);
    }

    // Left gutter (left ↔ middle)
    let left_gutter = DrawingArea::new();
    left_gutter.set_content_width(48);
    left_gutter.set_vexpand(true);

    {
        let ltv = left_pane.text_view.clone();
        let mtv = middle_pane.text_view.clone();
        let lb = left_buf.clone();
        let mb = middle_buf.clone();
        let ls = left_pane.scroll.clone();
        let ms = middle_pane.scroll.clone();
        let ch = left_chunks.clone();
        left_gutter.set_draw_func(move |area, cr, width, _height| {
            draw_gutter(
                cr,
                width as f64,
                &ltv,
                &mtv,
                &lb,
                &mb,
                &ls,
                &ms,
                area,
                &ch.borrow(),
            );
        });
    }

    // Left gutter click: → copies left→middle, ← copies middle→left
    {
        let gesture = GestureClick::new();
        let ltv = left_pane.text_view.clone();
        let mtv = middle_pane.text_view.clone();
        let lb = left_buf.clone();
        let mb = middle_buf.clone();
        let ls = left_pane.scroll.clone();
        let ms = middle_pane.scroll.clone();
        let ch = left_chunks.clone();
        let g = left_gutter.clone();
        gesture.connect_pressed(move |_, _, x, y| {
            handle_gutter_click(
                x,
                y,
                g.width() as f64,
                &ltv,
                &mtv,
                &lb,
                &mb,
                &ls,
                &ms,
                &g,
                &ch,
            );
        });
        left_gutter.add_controller(gesture);
    }

    // Left gutter right-click context menu
    {
        let lg_pending: Rc<Cell<Option<usize>>> = Rc::new(Cell::new(None));
        let lg_ctx = gio::SimpleActionGroup::new();
        {
            let action = gio::SimpleAction::new("copy-left-middle", None);
            let pc = lg_pending.clone();
            let ch = left_chunks.clone();
            let lb = left_buf.clone();
            let mb = middle_buf.clone();
            action.connect_activate(move |_, _| {
                if let Some(idx) = pc.get() {
                    let s = ch.borrow();
                    if let Some(c) = s.get(idx) {
                        copy_chunk(&lb, c.start_a, c.end_a, &mb, c.start_b, c.end_b);
                    }
                }
            });
            lg_ctx.add_action(&action);
        }
        {
            let action = gio::SimpleAction::new("copy-middle-left", None);
            let pc = lg_pending.clone();
            let ch = left_chunks.clone();
            let lb = left_buf.clone();
            let mb = middle_buf.clone();
            action.connect_activate(move |_, _| {
                if let Some(idx) = pc.get() {
                    let s = ch.borrow();
                    if let Some(c) = s.get(idx) {
                        copy_chunk(&mb, c.start_b, c.end_b, &lb, c.start_a, c.end_a);
                    }
                }
            });
            lg_ctx.add_action(&action);
        }
        left_gutter.insert_action_group("lgutter", Some(&lg_ctx));

        let lg_menu = gio::Menu::new();
        lg_menu.append(
            Some("Copy Left \u{2192} Middle"),
            Some("lgutter.copy-left-middle"),
        );
        lg_menu.append(
            Some("Copy Middle \u{2192} Left"),
            Some("lgutter.copy-middle-left"),
        );
        let lg_popover = PopoverMenu::from_model(Some(&lg_menu));
        lg_popover.set_parent(&left_gutter);
        lg_popover.set_has_arrow(false);

        let gesture = GestureClick::new();
        gesture.set_button(3);
        let ltv = left_pane.text_view.clone();
        let mtv = middle_pane.text_view.clone();
        let lb = left_buf.clone();
        let mb = middle_buf.clone();
        let ls = left_pane.scroll.clone();
        let ms = middle_pane.scroll.clone();
        let ch = left_chunks.clone();
        let g = left_gutter.clone();
        let pc = lg_pending;
        let pop = lg_popover;
        gesture.connect_pressed(move |_, _, x, y| {
            let snapshot = ch.borrow();
            for (idx, chunk) in snapshot.iter().enumerate() {
                if chunk.tag == DiffTag::Equal {
                    continue;
                }
                let lt = line_to_gutter_y(&ltv, &lb, chunk.start_a, &ls, &g);
                let lb_y = line_to_gutter_y(&ltv, &lb, chunk.end_a, &ls, &g);
                let mt = line_to_gutter_y(&mtv, &mb, chunk.start_b, &ms, &g);
                let mb_y = line_to_gutter_y(&mtv, &mb, chunk.end_b, &ms, &g);
                let top = lt.min(mt) - 6.0;
                let bottom = lb_y.max(mb_y) + 6.0;
                if y >= top && y <= bottom {
                    pc.set(Some(idx));
                    pop.set_pointing_to(Some(&gtk4::gdk::Rectangle::new(x as i32, y as i32, 1, 1)));
                    pop.popup();
                    return;
                }
            }
        });
        left_gutter.add_controller(gesture);
    }

    // Right gutter (middle ↔ right)
    let right_gutter = DrawingArea::new();
    right_gutter.set_content_width(48);
    right_gutter.set_vexpand(true);

    {
        let mtv = middle_pane.text_view.clone();
        let rtv = right_pane.text_view.clone();
        let mb = middle_buf.clone();
        let rb = right_buf.clone();
        let ms = middle_pane.scroll.clone();
        let rs = right_pane.scroll.clone();
        let ch = right_chunks.clone();
        right_gutter.set_draw_func(move |area, cr, width, _height| {
            draw_gutter(
                cr,
                width as f64,
                &mtv,
                &rtv,
                &mb,
                &rb,
                &ms,
                &rs,
                area,
                &ch.borrow(),
            );
        });
    }

    // Right gutter click: → copies middle→right, ← copies right→middle
    {
        let gesture = GestureClick::new();
        let mtv = middle_pane.text_view.clone();
        let rtv = right_pane.text_view.clone();
        let mb = middle_buf.clone();
        let rb = right_buf.clone();
        let ms = middle_pane.scroll.clone();
        let rs = right_pane.scroll.clone();
        let ch = right_chunks.clone();
        let g = right_gutter.clone();
        gesture.connect_pressed(move |_, _, x, y| {
            handle_gutter_click(
                x,
                y,
                g.width() as f64,
                &mtv,
                &rtv,
                &mb,
                &rb,
                &ms,
                &rs,
                &g,
                &ch,
            );
        });
        right_gutter.add_controller(gesture);
    }

    // Right gutter right-click context menu
    {
        let rg_pending: Rc<Cell<Option<usize>>> = Rc::new(Cell::new(None));
        let rg_ctx = gio::SimpleActionGroup::new();
        {
            let action = gio::SimpleAction::new("copy-middle-right", None);
            let pc = rg_pending.clone();
            let ch = right_chunks.clone();
            let mb = middle_buf.clone();
            let rb = right_buf.clone();
            action.connect_activate(move |_, _| {
                if let Some(idx) = pc.get() {
                    let s = ch.borrow();
                    if let Some(c) = s.get(idx) {
                        copy_chunk(&mb, c.start_a, c.end_a, &rb, c.start_b, c.end_b);
                    }
                }
            });
            rg_ctx.add_action(&action);
        }
        {
            let action = gio::SimpleAction::new("copy-right-middle", None);
            let pc = rg_pending.clone();
            let ch = right_chunks.clone();
            let mb = middle_buf.clone();
            let rb = right_buf.clone();
            action.connect_activate(move |_, _| {
                if let Some(idx) = pc.get() {
                    let s = ch.borrow();
                    if let Some(c) = s.get(idx) {
                        copy_chunk(&rb, c.start_b, c.end_b, &mb, c.start_a, c.end_a);
                    }
                }
            });
            rg_ctx.add_action(&action);
        }
        right_gutter.insert_action_group("rgutter", Some(&rg_ctx));

        let rg_menu = gio::Menu::new();
        rg_menu.append(
            Some("Copy Middle \u{2192} Right"),
            Some("rgutter.copy-middle-right"),
        );
        rg_menu.append(
            Some("Copy Right \u{2192} Middle"),
            Some("rgutter.copy-right-middle"),
        );
        let rg_popover = PopoverMenu::from_model(Some(&rg_menu));
        rg_popover.set_parent(&right_gutter);
        rg_popover.set_has_arrow(false);

        let gesture = GestureClick::new();
        gesture.set_button(3);
        let mtv = middle_pane.text_view.clone();
        let rtv = right_pane.text_view.clone();
        let mb = middle_buf.clone();
        let rb = right_buf.clone();
        let ms = middle_pane.scroll.clone();
        let rs = right_pane.scroll.clone();
        let ch = right_chunks.clone();
        let g = right_gutter.clone();
        let pc = rg_pending;
        let pop = rg_popover;
        gesture.connect_pressed(move |_, _, x, y| {
            let snapshot = ch.borrow();
            for (idx, chunk) in snapshot.iter().enumerate() {
                if chunk.tag == DiffTag::Equal {
                    continue;
                }
                let mt = line_to_gutter_y(&mtv, &mb, chunk.start_a, &ms, &g);
                let mb_y = line_to_gutter_y(&mtv, &mb, chunk.end_a, &ms, &g);
                let rt = line_to_gutter_y(&rtv, &rb, chunk.start_b, &rs, &g);
                let rb_y = line_to_gutter_y(&rtv, &rb, chunk.end_b, &rs, &g);
                let top = mt.min(rt) - 6.0;
                let bottom = mb_y.max(rb_y) + 6.0;
                if y >= top && y <= bottom {
                    pc.set(Some(idx));
                    pop.set_pointing_to(Some(&gtk4::gdk::Rectangle::new(x as i32, y as i32, 1, 1)));
                    pop.popup();
                    return;
                }
            }
        });
        right_gutter.add_controller(gesture);
    }

    // Text filter state
    let ignore_blanks: Rc<Cell<bool>> = Rc::new(Cell::new(false));
    let ignore_whitespace: Rc<Cell<bool>> = Rc::new(Cell::new(false));

    // Scroll sync
    setup_scroll_sync_3way(
        &left_pane.scroll,
        &middle_pane.scroll,
        &right_pane.scroll,
        &left_gutter,
        &right_gutter,
    );

    // ── Filler overlay drawing (3-way) ──────────────────────────
    let filler_lh: Rc<Cell<i32>> = Rc::new(Cell::new(get_line_height(&left_pane.text_view)));
    {
        let ltv = left_pane.text_view.clone();
        let ls = left_pane.scroll.clone();
        let lch = left_chunks.clone();
        let rch = right_chunks.clone();
        let flh = filler_lh.clone();
        left_pane
            .filler_overlay
            .set_draw_func(move |_area, cr, w, _h| {
                let lh = flh.get();
                draw_fillers(cr, w as f64, &ltv, &ls, &lch.borrow(), true, lh);
                draw_fillers(cr, w as f64, &ltv, &ls, &rch.borrow(), true, lh);
            });
    }
    {
        let rtv = right_pane.text_view.clone();
        let rs = right_pane.scroll.clone();
        let lch = left_chunks.clone();
        let rch = right_chunks.clone();
        let flh = filler_lh.clone();
        right_pane
            .filler_overlay
            .set_draw_func(move |_area, cr, w, _h| {
                let lh = flh.get();
                draw_fillers(cr, w as f64, &rtv, &rs, &rch.borrow(), false, lh);
                draw_fillers(cr, w as f64, &rtv, &rs, &lch.borrow(), false, lh);
            });
    }
    {
        let mtv = middle_pane.text_view.clone();
        let ms = middle_pane.scroll.clone();
        let lch = left_chunks.clone();
        let rch = right_chunks.clone();
        let flh = filler_lh.clone();
        middle_pane
            .filler_overlay
            .set_draw_func(move |_area, cr, w, _h| {
                let lh = flh.get();
                draw_fillers(cr, w as f64, &mtv, &ms, &lch.borrow(), false, lh);
                draw_fillers(cr, w as f64, &mtv, &ms, &rch.borrow(), true, lh);
            });
    }
    // Redraw filler overlays on scroll
    {
        let lf = left_pane.filler_overlay.clone();
        let mf = middle_pane.filler_overlay.clone();
        let rf = right_pane.filler_overlay.clone();
        for scroll in [&left_pane.scroll, &middle_pane.scroll, &right_pane.scroll] {
            let lf = lf.clone();
            let mf = mf.clone();
            let rf = rf.clone();
            scroll.vadjustment().connect_value_changed(move |_| {
                lf.queue_draw();
                mf.queue_draw();
                rf.queue_draw();
            });
        }
    }

    // ── Toolbar with chunk navigation ───────────────────────────
    let current_chunk: Rc<Cell<Option<(usize, bool)>>> = Rc::new(Cell::new(None));

    let chunk_label = Label::new(None);
    chunk_label.add_css_class("chunk-label");
    {
        let total = count_changes(&left_chunks.borrow()) + count_changes(&right_chunks.borrow());
        if total == 0 {
            chunk_label.set_label("No changes");
        } else {
            chunk_label.set_label(&format!("{total} changes"));
        }
    }

    let prev_btn = Button::from_icon_name("go-up-symbolic");
    prev_btn.set_tooltip_text(Some("Previous change (Alt+Up / Ctrl+E)"));
    let next_btn = Button::from_icon_name("go-down-symbolic");
    next_btn.set_tooltip_text(Some("Next change (Alt+Down / Ctrl+D)"));

    let nav_box = GtkBox::new(Orientation::Horizontal, 0);
    nav_box.add_css_class("linked");
    nav_box.append(&prev_btn);
    nav_box.append(&next_btn);

    // Conflict navigation buttons
    let prev_conflict_btn = Button::from_icon_name("go-up-symbolic");
    prev_conflict_btn.set_tooltip_text(Some("Previous conflict (Ctrl+J)"));
    let next_conflict_btn = Button::from_icon_name("go-down-symbolic");
    next_conflict_btn.set_tooltip_text(Some("Next conflict (Ctrl+K)"));
    let conflict_nav_box = GtkBox::new(Orientation::Horizontal, 0);
    conflict_nav_box.add_css_class("linked");
    conflict_nav_box.append(&prev_conflict_btn);
    conflict_nav_box.append(&next_conflict_btn);

    let conflict_label = Label::new(None);
    conflict_label.add_css_class("chunk-label");
    {
        let n = find_conflict_markers(&middle_buf).len();
        if n == 0 {
            conflict_label.set_label("No conflicts");
        } else {
            conflict_label.set_label(&format!("{n} conflicts"));
        }
    }

    let current_conflict: Rc<Cell<Option<usize>>> = Rc::new(Cell::new(None));

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

    // Go to line entry (hidden by default)
    let goto_entry = Entry::new();
    goto_entry.set_placeholder_text(Some("Line #"));
    goto_entry.set_width_chars(8);
    goto_entry.add_css_class("goto-entry");
    goto_entry.set_visible(false);
    {
        let av = active_view.clone();
        let ms = middle_pane.scroll.clone();
        let entry = goto_entry.clone();
        goto_entry.connect_activate(move |e| {
            if let Ok(line) = e.text().trim().parse::<usize>() {
                let tv = av.borrow().clone();
                let buf = tv.buffer();
                let target = line.saturating_sub(1);
                scroll_to_line(&tv, &buf, target, &ms);
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
    // Text filter toggles
    let blank_toggle = ToggleButton::with_label("Blanks");
    blank_toggle.set_tooltip_text(Some("Ignore blank lines"));
    let ws_toggle = ToggleButton::with_label("Spaces");
    ws_toggle.set_tooltip_text(Some("Ignore whitespace differences"));
    let filter_box = GtkBox::new(Orientation::Horizontal, 0);
    filter_box.add_css_class("linked");
    filter_box.append(&blank_toggle);
    filter_box.append(&ws_toggle);

    let merge_prefs_btn = Button::from_icon_name("preferences-system-symbolic");
    merge_prefs_btn.set_tooltip_text(Some("Preferences (Ctrl+,)"));
    merge_prefs_btn.set_action_name(Some("win.prefs"));

    toolbar.append(&undo_redo_box);
    toolbar.append(&nav_box);
    toolbar.append(&chunk_label);
    toolbar.append(&conflict_nav_box);
    toolbar.append(&conflict_label);
    toolbar.append(&goto_entry);
    toolbar.append(&filter_box);
    toolbar.append(&merge_prefs_btn);

    // Navigate helper for merge view
    let navigate_merge_chunk = |lch: &[DiffChunk],
                                rch: &[DiffChunk],
                                cur: &Rc<Cell<Option<(usize, bool)>>>,
                                direction: i32,
                                mtv: &TextView,
                                mb: &TextBuffer,
                                ms: &ScrolledWindow| {
        let all = merge_change_indices(lch, rch);
        if all.is_empty() {
            return;
        }
        let next = match cur.get() {
            Some(cur_val) => {
                let pos = all.iter().position(|v| *v == cur_val);
                match pos {
                    Some(p) => {
                        if direction > 0 {
                            all.get(p + 1).or(all.first())
                        } else if p > 0 {
                            all.get(p - 1)
                        } else {
                            all.last()
                        }
                    }
                    None => {
                        if direction > 0 {
                            all.first()
                        } else {
                            all.last()
                        }
                    }
                }
            }
            None => {
                if direction > 0 {
                    all.first()
                } else {
                    all.last()
                }
            }
        };
        if let Some(&(idx, is_right)) = next {
            cur.set(Some((idx, is_right)));
            // Scroll middle pane to the chunk
            let line = if is_right {
                rch[idx].start_a
            } else {
                lch[idx].start_b
            };
            scroll_to_line(mtv, mb, line, ms);
        }
    };

    let update_merge_label =
        |lbl: &Label, lch: &[DiffChunk], rch: &[DiffChunk], cur: Option<(usize, bool)>| {
            let all = merge_change_indices(lch, rch);
            let total = all.len();
            if total == 0 {
                lbl.set_label("No changes");
                return;
            }
            match cur {
                Some(cur_val) => {
                    if let Some(pos) = all.iter().position(|v| *v == cur_val) {
                        lbl.set_label(&format!("Change {} of {}", pos + 1, total));
                    } else {
                        lbl.set_label(&format!("{total} changes"));
                    }
                }
                None => lbl.set_label(&format!("{total} changes")),
            }
        };

    // Prev chunk
    {
        let lch = left_chunks.clone();
        let rch = right_chunks.clone();
        let cur = current_chunk.clone();
        let mtv = middle_pane.text_view.clone();
        let mb = middle_buf.clone();
        let ms = middle_pane.scroll.clone();
        let lbl = chunk_label.clone();
        let nav = navigate_merge_chunk;
        let upd = update_merge_label;
        prev_btn.connect_clicked(move |_| {
            nav(&lch.borrow(), &rch.borrow(), &cur, -1, &mtv, &mb, &ms);
            upd(&lbl, &lch.borrow(), &rch.borrow(), cur.get());
        });
    }

    // Next chunk
    {
        let lch = left_chunks.clone();
        let rch = right_chunks.clone();
        let cur = current_chunk.clone();
        let mtv = middle_pane.text_view.clone();
        let mb = middle_buf.clone();
        let ms = middle_pane.scroll.clone();
        let lbl = chunk_label.clone();
        let nav = navigate_merge_chunk;
        let upd = update_merge_label;
        next_btn.connect_clicked(move |_| {
            nav(&lch.borrow(), &rch.borrow(), &cur, 1, &mtv, &mb, &ms);
            upd(&lbl, &lch.borrow(), &rch.borrow(), cur.get());
        });
    }

    // Navigate conflict helper — jumps between `<<<<<<<` markers in middle buf.
    let navigate_conflict = |cur: &Rc<Cell<Option<usize>>>,
                             direction: i32,
                             mtv: &TextView,
                             mb: &TextBuffer,
                             ms: &ScrolledWindow| {
        let markers = find_conflict_markers(mb);
        if markers.is_empty() {
            return;
        }
        let next = match cur.get() {
            Some(cur_line) => {
                let pos = markers.iter().position(|&l| l == cur_line);
                match pos {
                    Some(p) => {
                        if direction > 0 {
                            markers.get(p + 1).or(markers.first())
                        } else if p > 0 {
                            markers.get(p - 1)
                        } else {
                            markers.last()
                        }
                    }
                    None => {
                        if direction > 0 {
                            markers.first()
                        } else {
                            markers.last()
                        }
                    }
                }
            }
            None => {
                if direction > 0 {
                    markers.first()
                } else {
                    markers.last()
                }
            }
        };
        if let Some(&line) = next {
            cur.set(Some(line));
            scroll_to_line(mtv, mb, line, ms);
        }
    };

    let update_conflict_label = |lbl: &Label, mb: &TextBuffer, cur: Option<usize>| {
        let markers = find_conflict_markers(mb);
        let total = markers.len();
        if total == 0 {
            lbl.set_label("No conflicts");
            return;
        }
        match cur {
            Some(cur_line) => {
                if let Some(pos) = markers.iter().position(|&l| l == cur_line) {
                    lbl.set_label(&format!("Conflict {} of {}", pos + 1, total));
                } else {
                    lbl.set_label(&format!("{total} conflicts"));
                }
            }
            None => lbl.set_label(&format!("{total} conflicts")),
        }
    };

    // Prev conflict
    {
        let cur = current_conflict.clone();
        let mtv = middle_pane.text_view.clone();
        let mb = middle_buf.clone();
        let ms = middle_pane.scroll.clone();
        let lbl = conflict_label.clone();
        let nav = navigate_conflict;
        let upd = update_conflict_label;
        prev_conflict_btn.connect_clicked(move |_| {
            nav(&cur, -1, &mtv, &mb, &ms);
            upd(&lbl, &mb, cur.get());
        });
    }

    // Next conflict
    {
        let cur = current_conflict.clone();
        let mtv = middle_pane.text_view.clone();
        let mb = middle_buf.clone();
        let ms = middle_pane.scroll.clone();
        let lbl = conflict_label.clone();
        let nav = navigate_conflict;
        let upd = update_conflict_label;
        next_conflict_btn.connect_clicked(move |_| {
            nav(&cur, 1, &mtv, &mb, &ms);
            upd(&lbl, &mb, cur.get());
        });
    }

    // ── Chunk maps for merge view ────────────────────────────────
    let left_chunk_map = DrawingArea::new();
    left_chunk_map.set_content_width(12);
    left_chunk_map.set_vexpand(true);
    {
        let lb = left_buf.clone();
        let ls = left_pane.scroll.clone();
        let ch = left_chunks.clone();
        left_chunk_map.set_draw_func(move |_area, cr, _w, h| {
            draw_chunk_map(cr, 12.0, h as f64, lb.line_count(), &ls, &ch.borrow(), true);
        });
    }
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
        let ch = right_chunks.clone();
        right_chunk_map.set_draw_func(move |_area, cr, _w, h| {
            draw_chunk_map(
                cr,
                12.0,
                h as f64,
                rb.line_count(),
                &rs,
                &ch.borrow(),
                false,
            );
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

    // Redraw chunk maps on scroll
    {
        let lcm = left_chunk_map.clone();
        let rcm = right_chunk_map.clone();
        middle_pane
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
            let lg = left_gutter.clone();
            let rg = right_gutter.clone();
            let lcm = left_chunk_map.clone();
            let rcm = right_chunk_map.clone();
            let lbl = chunk_label.clone();
            let clbl = conflict_label.clone();
            let lch = left_chunks.clone();
            let rch = right_chunks.clone();
            let mb = middle_buf.clone();
            let cur = current_chunk.clone();
            let ccur = current_conflict.clone();
            let lf = left_pane.filler_overlay.clone();
            let mf = middle_pane.filler_overlay.clone();
            let rf = right_pane.filler_overlay.clone();
            move || {
                let lg = lg.clone();
                let rg = rg.clone();
                let lcm = lcm.clone();
                let rcm = rcm.clone();
                let lbl = lbl.clone();
                let clbl = clbl.clone();
                let lch = lch.clone();
                let rch = rch.clone();
                let mb = mb.clone();
                let cur = cur.clone();
                let ccur = ccur.clone();
                let lf = lf.clone();
                let mf = mf.clone();
                let rf = rf.clone();
                move || {
                    lg.queue_draw();
                    rg.queue_draw();
                    lcm.queue_draw();
                    rcm.queue_draw();
                    lf.queue_draw();
                    mf.queue_draw();
                    rf.queue_draw();
                    cur.set(None);
                    update_merge_label(&lbl, &lch.borrow(), &rch.borrow(), None);
                    ccur.set(None);
                    let n = find_conflict_markers(&mb).len();
                    if n == 0 {
                        clbl.set_label("No conflicts");
                    } else {
                        clbl.set_label(&format!("{n} conflicts"));
                    }
                }
            }
        };

        let pending = Rc::new(Cell::new(false));
        let connect_refresh = |buf: &TextBuffer| {
            let lb = left_buf.clone();
            let mb = middle_buf.clone();
            let rb = right_buf.clone();
            let ltv = left_pane.text_view.clone();
            let lch = left_chunks.clone();
            let rch = right_chunks.clone();
            let flh = filler_lh.clone();
            let p = pending.clone();
            let ib = ignore_blanks.clone();
            let iw = ignore_whitespace.clone();
            let make_cb = make_on_complete.clone();
            buf.connect_changed(move |_| {
                if !p.get() {
                    p.set(true);
                    let lb = lb.clone();
                    let mb = mb.clone();
                    let rb = rb.clone();
                    let ltv = ltv.clone();
                    let lch = lch.clone();
                    let rch = rch.clone();
                    let flh = flh.clone();
                    let p = p.clone();
                    let ib = ib.clone();
                    let iw = iw.clone();
                    let cb = make_cb();
                    gtk4::glib::idle_add_local_once(move || {
                        refresh_merge_diffs(
                            &lb,
                            &mb,
                            &rb,
                            &ltv,
                            &lch,
                            &rch,
                            &flh,
                            cb,
                            ib.get(),
                            iw.get(),
                            &p,
                        );
                    });
                }
            });
        };
        connect_refresh(&left_buf);
        connect_refresh(&middle_buf);
        connect_refresh(&right_buf);

        // Initial async diff (must be after chunk_label + chunk_maps exist)
        if !any_binary && (!left_identical || !right_identical) {
            let lb = left_buf.clone();
            let mb = middle_buf.clone();
            let rb = right_buf.clone();
            let ltv = left_pane.text_view.clone();
            let lch = left_chunks.clone();
            let rch = right_chunks.clone();
            let flh = filler_lh.clone();
            let on_complete = make_on_complete();
            pending.set(true);
            let p = pending.clone();
            gtk4::glib::spawn_future_local(async move {
                let (new_left, new_right) = gio::spawn_blocking(move || {
                    let nl = if left_identical {
                        Vec::new()
                    } else {
                        myers::diff_lines(&left_content, &middle_content)
                    };
                    let nr = if right_identical {
                        Vec::new()
                    } else {
                        myers::diff_lines(&middle_content, &right_content)
                    };
                    (nl, nr)
                })
                .await
                .unwrap_or_default();

                let lh = get_line_height(&ltv);
                flh.set(lh);
                apply_merge_tags(&lb, &mb, &rb, &new_left, &new_right);
                apply_merge_filler_tags(&lb, &mb, &rb, &new_left, &new_right, lh);
                *lch.borrow_mut() = new_left;
                *rch.borrow_mut() = new_right;
                on_complete();
                p.set(false);
            });
        }

        // Toggle handlers for filter buttons
        {
            let ib = ignore_blanks.clone();
            let iw = ignore_whitespace.clone();
            let lb = left_buf.clone();
            let mb = middle_buf.clone();
            let rb = right_buf.clone();
            let ltv = left_pane.text_view.clone();
            let lch = left_chunks.clone();
            let rch = right_chunks.clone();
            let flh = filler_lh.clone();
            let make_cb = make_on_complete.clone();
            let dummy = Rc::new(Cell::new(true));
            blank_toggle.connect_toggled(move |btn| {
                ib.set(btn.is_active());
                dummy.set(true);
                refresh_merge_diffs(
                    &lb,
                    &mb,
                    &rb,
                    &ltv,
                    &lch,
                    &rch,
                    &flh,
                    make_cb(),
                    ib.get(),
                    iw.get(),
                    &dummy,
                );
            });
        }
        {
            let ib = ignore_blanks.clone();
            let iw = ignore_whitespace.clone();
            let lb = left_buf.clone();
            let mb = middle_buf.clone();
            let rb = right_buf.clone();
            let ltv = left_pane.text_view.clone();
            let lch = left_chunks.clone();
            let rch = right_chunks.clone();
            let flh = filler_lh.clone();
            let dummy = Rc::new(Cell::new(true));
            ws_toggle.connect_toggled(move |btn| {
                iw.set(btn.is_active());
                dummy.set(true);
                refresh_merge_diffs(
                    &lb,
                    &mb,
                    &rb,
                    &ltv,
                    &lch,
                    &rch,
                    &flh,
                    make_on_complete(),
                    ib.get(),
                    iw.get(),
                    &dummy,
                );
            });
        }
    }

    // ── Find bar for merge view ───────────────────────────────────
    let find_entry = Entry::new();
    find_entry.set_placeholder_text(Some("Find (Ctrl+F)"));
    find_entry.set_hexpand(true);

    let replace_entry = Entry::new();
    replace_entry.set_placeholder_text(Some("Replace"));
    replace_entry.set_hexpand(true);

    let find_prev_btn = Button::from_icon_name("go-up-symbolic");
    let find_next_btn = Button::from_icon_name("go-down-symbolic");
    let match_label = Label::new(None);
    match_label.add_css_class("chunk-label");
    let find_close_btn = Button::from_icon_name("window-close-symbolic");
    find_close_btn.set_has_frame(false);

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

    // Search logic
    {
        let lb = left_buf.clone();
        let mb = middle_buf.clone();
        let rb = right_buf.clone();
        let ml = match_label.clone();
        find_entry.connect_changed(move |e| {
            let needle = e.text().to_string();
            let total = highlight_search_matches(&lb, &needle)
                + highlight_search_matches(&mb, &needle)
                + highlight_search_matches(&rb, &needle);
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
        let ms = middle_pane.scroll.clone();
        find_next_btn.connect_clicked(move |_| {
            let needle = fe.text().to_string();
            let tv = av.borrow().clone();
            let buf = tv.buffer();
            let cursor = buf.iter_at_mark(&buf.get_insert());
            if let Some((start, end)) = find_next_match(&buf, &needle, &cursor, true) {
                buf.select_range(&start, &end);
                scroll_to_line(&tv, &buf, start.line() as usize, &ms);
            }
        });
    }

    // Find prev
    {
        let av = active_view.clone();
        let fe = find_entry.clone();
        let ms = middle_pane.scroll.clone();
        find_prev_btn.connect_clicked(move |_| {
            let needle = fe.text().to_string();
            let tv = av.borrow().clone();
            let buf = tv.buffer();
            let cursor = buf.iter_at_mark(&buf.get_insert());
            if let Some((start, end)) = find_next_match(&buf, &needle, &cursor, false) {
                buf.select_range(&start, &end);
                scroll_to_line(&tv, &buf, start.line() as usize, &ms);
            }
        });
    }

    // Enter in find entry = find next
    {
        let av = active_view.clone();
        let ms = middle_pane.scroll.clone();
        find_entry.connect_activate(move |e| {
            let needle = e.text().to_string();
            let tv = av.borrow().clone();
            let buf = tv.buffer();
            let cursor = buf.iter_at_mark(&buf.get_insert());
            if let Some((start, end)) = find_next_match(&buf, &needle, &cursor, true) {
                buf.select_range(&start, &end);
                scroll_to_line(&tv, &buf, start.line() as usize, &ms);
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
        let mb = middle_buf.clone();
        let rb = right_buf.clone();
        let find_e = find_entry.clone();
        let repl_e = replace_entry.clone();
        replace_all_btn.connect_clicked(move |_| {
            let needle = find_e.text().to_string();
            let replacement = repl_e.text().to_string();
            if needle.is_empty() {
                return;
            }
            for buf in [&lb, &mb, &rb] {
                let text = buf
                    .text(&buf.start_iter(), &buf.end_iter(), false)
                    .to_string();
                let new_text = text.replace(&needle, &replacement);
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
        let mb = middle_buf.clone();
        let rb = right_buf.clone();
        find_close_btn.connect_clicked(move |_| {
            fr.set_reveal_child(false);
            clear_search_tags(&lb);
            clear_search_tags(&mb);
            clear_search_tags(&rb);
        });
    }

    // Escape in find entry
    {
        let fr = find_revealer.clone();
        let lb = left_buf.clone();
        let mb = middle_buf.clone();
        let rb = right_buf.clone();
        let key_ctl = EventControllerKey::new();
        key_ctl.connect_key_pressed(move |_, key, _, _| {
            if key == gtk4::gdk::Key::Escape {
                fr.set_reveal_child(false);
                clear_search_tags(&lb);
                clear_search_tags(&mb);
                clear_search_tags(&rb);
                return gtk4::glib::Propagation::Stop;
            }
            gtk4::glib::Propagation::Proceed
        });
        find_entry.add_controller(key_ctl);
    }

    // Layout: [chunk_map | left pane | left_gutter | middle pane | right_gutter | right pane | chunk_map]
    let diff_row = GtkBox::new(Orientation::Horizontal, 0);
    left_pane.container.set_hexpand(true);
    middle_pane.container.set_hexpand(true);
    right_pane.container.set_hexpand(true);
    diff_row.append(&left_chunk_map);
    diff_row.append(&left_pane.container);
    diff_row.append(&left_gutter);
    diff_row.append(&middle_pane.container);
    diff_row.append(&right_gutter);
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
        let lch = left_chunks.clone();
        let rch = right_chunks.clone();
        let cur = current_chunk.clone();
        let mtv = middle_pane.text_view.clone();
        let mb = middle_buf.clone();
        let ms = middle_pane.scroll.clone();
        let lbl = chunk_label.clone();
        action.connect_activate(move |_, _| {
            navigate_merge_chunk(&lch.borrow(), &rch.borrow(), &cur, -1, &mtv, &mb, &ms);
            update_merge_label(&lbl, &lch.borrow(), &rch.borrow(), cur.get());
        });
        action_group.add_action(&action);
    }
    {
        let action = gio::SimpleAction::new("next-chunk", None);
        let lch = left_chunks.clone();
        let rch = right_chunks.clone();
        let cur = current_chunk.clone();
        let mtv = middle_pane.text_view.clone();
        let mb = middle_buf.clone();
        let ms = middle_pane.scroll.clone();
        let lbl = chunk_label.clone();
        action.connect_activate(move |_, _| {
            navigate_merge_chunk(&lch.borrow(), &rch.borrow(), &cur, 1, &mtv, &mb, &ms);
            update_merge_label(&lbl, &lch.borrow(), &rch.borrow(), cur.get());
        });
        action_group.add_action(&action);
    }
    {
        let action = gio::SimpleAction::new("prev-conflict", None);
        let cur = current_conflict.clone();
        let mtv = middle_pane.text_view.clone();
        let mb = middle_buf.clone();
        let ms = middle_pane.scroll.clone();
        let lbl = conflict_label.clone();
        action.connect_activate(move |_, _| {
            navigate_conflict(&cur, -1, &mtv, &mb, &ms);
            update_conflict_label(&lbl, &mb, cur.get());
        });
        action_group.add_action(&action);
    }
    {
        let action = gio::SimpleAction::new("next-conflict", None);
        let cur = current_conflict.clone();
        let mtv = middle_pane.text_view.clone();
        let mb = middle_buf.clone();
        let ms = middle_pane.scroll.clone();
        let lbl = conflict_label.clone();
        action.connect_activate(move |_, _| {
            navigate_conflict(&cur, 1, &mtv, &mb, &ms);
            update_conflict_label(&lbl, &mb, cur.get());
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
        let ms = middle_pane.scroll.clone();
        action.connect_activate(move |_, _| {
            let needle = fe.text().to_string();
            let tv = av.borrow().clone();
            let buf = tv.buffer();
            let cursor = buf.iter_at_mark(&buf.get_insert());
            if let Some((start, end)) = find_next_match(&buf, &needle, &cursor, true) {
                buf.select_range(&start, &end);
                scroll_to_line(&tv, &buf, start.line() as usize, &ms);
            }
        });
        action_group.add_action(&action);
    }
    // Find prev (Shift+F3)
    {
        let action = gio::SimpleAction::new("find-prev", None);
        let av = active_view.clone();
        let fe = find_entry.clone();
        let ms = middle_pane.scroll.clone();
        action.connect_activate(move |_, _| {
            let needle = fe.text().to_string();
            let tv = av.borrow().clone();
            let buf = tv.buffer();
            let cursor = buf.iter_at_mark(&buf.get_insert());
            if let Some((start, end)) = find_next_match(&buf, &needle, &cursor, false) {
                buf.select_range(&start, &end);
                scroll_to_line(&tv, &buf, start.line() as usize, &ms);
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

    MergeViewResult {
        widget,
        left_buf,
        middle_buf,
        right_buf,
        middle_save: middle_pane.save_btn,
        action_group,
    }
}

pub(super) fn build_merge_window(
    app: &Application,
    left_path: std::path::PathBuf,
    middle_path: std::path::PathBuf,
    right_path: std::path::PathBuf,
    output: Option<std::path::PathBuf>,
    labels: &[String],
    settings: &Rc<RefCell<Settings>>,
) {
    let mv = build_merge_view(
        &left_path,
        &middle_path,
        &right_path,
        output.as_deref(),
        labels,
        settings,
    );

    // "Save Merged" button when --output is set
    if let Some(ref out_path) = output {
        let save_btn = Button::with_label("Save Merged");
        save_btn.set_tooltip_text(Some(&format!("Save to {}", out_path.display())));
        let mb = mv.middle_buf.clone();
        let op = out_path.clone();
        let btn_ref = save_btn.clone();
        save_btn.connect_clicked(move |_| {
            let text = mb.text(&mb.start_iter(), &mb.end_iter(), false);
            save_file(&op, text.as_str(), &btn_ref);
        });
        // Re-enable after edits
        {
            let btn = save_btn.clone();
            mv.middle_buf.connect_changed(move |_| {
                btn.set_sensitive(true);
            });
        }
        // Insert the save button into toolbar (first child of the widget is the toolbar)
        if let Some(toolbar) = mv.widget.first_child()
            && let Some(toolbar_box) = toolbar.downcast_ref::<GtkBox>()
        {
            toolbar_box.append(&save_btn);
        }
    }

    // File watcher on all 3 files
    let merge_watcher_alive = Rc::new(Cell::new(true));
    let (fs_tx, fs_rx) = mpsc::channel::<()>();
    let merge_watcher = {
        use notify::{RecursiveMode, Watcher};
        let op = output.clone().unwrap_or_else(|| middle_path.clone());
        let mut w =
            notify::recommended_watcher(move |res: Result<notify::Event, notify::Error>| {
                if res.is_ok() {
                    let _ = fs_tx.send(());
                }
            })
            .expect("Failed to create file watcher");
        for p in [&left_path, &op, &right_path] {
            if let Some(parent) = p.parent() {
                w.watch(parent, RecursiveMode::NonRecursive).ok();
            }
        }
        w
    };

    // Poll for filesystem changes and reload
    {
        let lb = mv.left_buf.clone();
        let mb = mv.middle_buf.clone();
        let rb = mv.right_buf.clone();
        let lp = left_path.clone();
        let mp = output.clone().unwrap_or_else(|| middle_path.clone());
        let rp = right_path.clone();
        let m_save = mv.middle_save.clone();
        let alive = merge_watcher_alive.clone();
        let loading = Rc::new(Cell::new(false));
        let dirty = Rc::new(Cell::new(false));
        gtk4::glib::timeout_add_local(Duration::from_millis(500), move || {
            let _ = &merge_watcher; // prevent drop; watcher lives until closure is dropped
            if !alive.get() {
                return gtk4::glib::ControlFlow::Break;
            }
            while fs_rx.try_recv().is_ok() {
                dirty.set(true);
            }
            if dirty.get()
                && !loading.get()
                && !is_saving(&[&lp, &mp, &rp])
                && !m_save.is_sensitive()
            {
                dirty.set(false);
                loading.set(true);
                let lp2 = lp.clone();
                let mp2 = mp.clone();
                let rp2 = rp.clone();
                let lb2 = lb.clone();
                let mb2 = mb.clone();
                let rb2 = rb.clone();
                let m_save2 = m_save.clone();
                let loading2 = loading.clone();
                let dirty2 = dirty.clone();
                gtk4::glib::spawn_future_local(async move {
                    let (left_content, middle_content, right_content) =
                        gio::spawn_blocking(move || {
                            (
                                read_file_for_reload(&lp2),
                                read_file_for_reload(&mp2),
                                read_file_for_reload(&rp2),
                            )
                        })
                        .await
                        .unwrap();
                    loading2.set(false);
                    // Retry on next tick if any file became binary or unreadable
                    let (Some(left_content), Some(middle_content), Some(right_content)) =
                        (left_content, middle_content, right_content)
                    else {
                        dirty2.set(true);
                        return;
                    };
                    let cur_l = lb2.text(&lb2.start_iter(), &lb2.end_iter(), false);
                    let cur_m = mb2.text(&mb2.start_iter(), &mb2.end_iter(), false);
                    let cur_r = rb2.text(&rb2.start_iter(), &rb2.end_iter(), false);
                    if cur_l.as_str() != left_content
                        || cur_m.as_str() != middle_content
                        || cur_r.as_str() != right_content
                    {
                        // set_text triggers connect_changed which schedules
                        // refresh_merge_diffs with the current filter state.
                        lb2.set_text(&left_content);
                        mb2.set_text(&middle_content);
                        rb2.set_text(&right_content);
                        m_save2.set_sensitive(false);
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
    let middle_name = middle_path.file_name().map_or_else(
        || middle_path.display().to_string(),
        |n| n.to_string_lossy().into_owned(),
    );
    let right_name = right_path.file_name().map_or_else(
        || right_path.display().to_string(),
        |n| n.to_string_lossy().into_owned(),
    );
    let title = format!("{left_name} — {middle_name} — {right_name}");

    let window = ApplicationWindow::builder()
        .application(app)
        .title(&title)
        .default_width(1200)
        .default_height(600)
        .child(&mv.widget)
        .build();
    window.insert_action_group("diff", Some(&mv.action_group));

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
    // Close-tab action (Ctrl+W) — close merge window
    {
        let action = gio::SimpleAction::new("close-tab", None);
        let w = window.clone();
        action.connect_activate(move |_, _| w.close());
        win_actions.add_action(&action);
    }
    window.insert_action_group("win", Some(&win_actions));

    // Unsaved-changes guard on window close button
    {
        let ms = mv.middle_save.clone();
        let save_path = output
            .as_deref()
            .unwrap_or(&middle_path)
            .display()
            .to_string();
        window.connect_close_request(move |w| {
            handle_close_request(w, vec![(save_path.clone(), ms.clone())])
        });
    }

    if let Some(gtk_app) = window.application() {
        gtk_app.set_accels_for_action("diff.prev-chunk", &["<Alt>Up", "<Ctrl>e"]);
        gtk_app.set_accels_for_action("diff.next-chunk", &["<Alt>Down", "<Ctrl>d"]);
        gtk_app.set_accels_for_action("diff.prev-conflict", &["<Ctrl>j"]);
        gtk_app.set_accels_for_action("diff.next-conflict", &["<Ctrl>k"]);
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
        merge_watcher_alive.set(false);
    });

    window.present();
}

#[cfg(test)]
mod tests {
    use super::*;

    // ── merge_change_indices ─────────────────────────────────────

    #[test]
    fn merge_change_indices_empty() {
        let result = merge_change_indices(&[], &[]);
        assert!(result.is_empty());
    }

    #[test]
    fn merge_change_indices_only_equal() {
        let left = vec![DiffChunk {
            tag: DiffTag::Equal,
            start_a: 0,
            end_a: 5,
            start_b: 0,
            end_b: 5,
        }];
        let right = left.clone();
        let result = merge_change_indices(&left, &right);
        assert!(result.is_empty());
    }

    #[test]
    fn merge_change_indices_left_only() {
        let left = vec![
            DiffChunk {
                tag: DiffTag::Equal,
                start_a: 0,
                end_a: 2,
                start_b: 0,
                end_b: 2,
            },
            DiffChunk {
                tag: DiffTag::Replace,
                start_a: 2,
                end_a: 4,
                start_b: 2,
                end_b: 4,
            },
        ];
        let right: Vec<DiffChunk> = vec![];
        let result = merge_change_indices(&left, &right);
        assert_eq!(result.len(), 1);
        assert_eq!(result[0], (1, false)); // index 1 in left, is_right=false
    }

    #[test]
    fn merge_change_indices_sorted_by_middle_line() {
        let left = vec![DiffChunk {
            tag: DiffTag::Replace,
            start_a: 0,
            end_a: 2,
            start_b: 10, // middle line 10
            end_b: 12,
        }];
        let right = vec![DiffChunk {
            tag: DiffTag::Insert,
            start_a: 5, // middle line 5
            end_a: 5,
            start_b: 0,
            end_b: 2,
        }];
        let result = merge_change_indices(&left, &right);
        assert_eq!(result.len(), 2);
        // Right chunk (middle line 5) should come first
        assert_eq!(result[0], (0, true));
        // Left chunk (middle line 10) should come second
        assert_eq!(result[1], (0, false));
    }
}
