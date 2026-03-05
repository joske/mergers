#[allow(clippy::wildcard_imports)]
use super::*;

// ─── 3-way merge view ───────────────────────────────────────────────────────

pub(super) struct MergeViewResult {
    pub(super) widget: GtkBox,
    pub(super) left_buf: TextBuffer,
    pub(super) middle_buf: TextBuffer,
    pub(super) right_buf: TextBuffer,
    pub(super) middle_save: Button,
    pub(super) middle_tab_path: Rc<RefCell<String>>,
    pub(super) action_group: gio::SimpleActionGroup,
}

#[allow(clippy::too_many_arguments)]
fn refresh_merge_diffs(
    left_buf: &TextBuffer,
    middle_buf: &TextBuffer,
    right_buf: &TextBuffer,
    left_chunks: &Rc<RefCell<Vec<DiffChunk>>>,
    right_chunks: &Rc<RefCell<Vec<DiffChunk>>>,
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

        *lch.borrow_mut() = new_left;
        *rch.borrow_mut() = new_right;
        on_complete();
        p.set(false);
    });
}

/// 3-way scroll sync using chunk-based mapping (matching meld's influence-through-middle).
///
/// `left_chunks`: left(A) vs middle(B).  `right_chunks`: middle(A) vs right(B).
#[allow(clippy::too_many_arguments)]
fn setup_scroll_sync_3way(
    left_scroll: &ScrolledWindow,
    middle_scroll: &ScrolledWindow,
    right_scroll: &ScrolledWindow,
    left_tv: &TextView,
    middle_tv: &TextView,
    right_tv: &TextView,
    left_buf: &TextBuffer,
    middle_buf: &TextBuffer,
    right_buf: &TextBuffer,
    left_chunks: &Rc<RefCell<Vec<DiffChunk>>>,
    right_chunks: &Rc<RefCell<Vec<DiffChunk>>>,
    left_gutter: &DrawingArea,
    right_gutter: &DrawingArea,
) {
    let syncing = Rc::new(Cell::new(false));

    // Vertical: Left → Middle (left_chunks), then Middle → Right (right_chunks)
    {
        let ms = middle_scroll.clone();
        let rs = right_scroll.clone();
        let ltv = left_tv.clone();
        let mtv = middle_tv.clone();
        let rtv = right_tv.clone();
        let mb = middle_buf.clone();
        let rb = right_buf.clone();
        let lch = left_chunks.clone();
        let rch = right_chunks.clone();
        let lg = left_gutter.clone();
        let rg = right_gutter.clone();
        let s = syncing.clone();
        left_scroll.vadjustment().connect_value_changed(move |adj| {
            lg.queue_draw();
            rg.queue_draw();
            if !s.get() {
                s.set(true);
                sync_vscroll(adj, &ltv, &ms.vadjustment(), &mtv, &mb, &lch.borrow(), true);
                sync_vscroll(
                    &ms.vadjustment(),
                    &mtv,
                    &rs.vadjustment(),
                    &rtv,
                    &rb,
                    &rch.borrow(),
                    true,
                );
                s.set(false);
            }
        });
    }

    // Vertical: Middle → Left (left_chunks) + Right (right_chunks)
    {
        let ls = left_scroll.clone();
        let rs = right_scroll.clone();
        let ltv = left_tv.clone();
        let mtv = middle_tv.clone();
        let rtv = right_tv.clone();
        let lb = left_buf.clone();
        let rb = right_buf.clone();
        let lch = left_chunks.clone();
        let rch = right_chunks.clone();
        let lg = left_gutter.clone();
        let rg = right_gutter.clone();
        let s = syncing.clone();
        middle_scroll
            .vadjustment()
            .connect_value_changed(move |adj| {
                lg.queue_draw();
                rg.queue_draw();
                if !s.get() {
                    s.set(true);
                    sync_vscroll(
                        adj,
                        &mtv,
                        &ls.vadjustment(),
                        &ltv,
                        &lb,
                        &lch.borrow(),
                        false,
                    );
                    sync_vscroll(adj, &mtv, &rs.vadjustment(), &rtv, &rb, &rch.borrow(), true);
                    s.set(false);
                }
            });
    }

    // Vertical: Right → Middle (right_chunks), then Middle → Left (left_chunks)
    {
        let ls = left_scroll.clone();
        let ms = middle_scroll.clone();
        let ltv = left_tv.clone();
        let mtv = middle_tv.clone();
        let rtv = right_tv.clone();
        let lb = left_buf.clone();
        let mb = middle_buf.clone();
        let lch = left_chunks.clone();
        let rch = right_chunks.clone();
        let lg = left_gutter.clone();
        let rg = right_gutter.clone();
        let s = syncing.clone();
        right_scroll
            .vadjustment()
            .connect_value_changed(move |adj| {
                lg.queue_draw();
                rg.queue_draw();
                if !s.get() {
                    s.set(true);
                    sync_vscroll(
                        adj,
                        &rtv,
                        &ms.vadjustment(),
                        &mtv,
                        &mb,
                        &rch.borrow(),
                        false,
                    );
                    sync_vscroll(
                        &ms.vadjustment(),
                        &mtv,
                        &ls.vadjustment(),
                        &ltv,
                        &lb,
                        &lch.borrow(),
                        false,
                    );
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
    super::merge_state::merge_change_indices(left_chunks, right_chunks)
}

/// Find line numbers of `<<<<<<<` conflict markers in a buffer.
fn find_conflict_markers(buf: &TextBuffer) -> Vec<usize> {
    let text = buf.text(&buf.start_iter(), &buf.end_iter(), false);
    super::merge_state::find_conflict_markers_in_text(&text)
}

/// Find the opening `<<<<<<<` marker line for the conflict block at `cursor_line`.
fn conflict_at_cursor(buf: &TextBuffer, cursor_line: usize) -> Option<usize> {
    let text = buf.text(&buf.start_iter(), &buf.end_iter(), false);
    super::merge_state::conflict_at_cursor(&text, cursor_line)
}

pub(super) fn build_merge_view(
    left_path: &Path,
    middle_path: &Path,
    right_path: &Path,
    labels: &[String],
    settings: &Rc<RefCell<Settings>>,
) -> MergeViewResult {
    let s = settings.borrow();
    let (left_content, left_binary) = read_file_content(left_path);
    let (middle_content, middle_binary) = read_file_content(middle_path);
    let (right_content, right_binary) = read_file_content(right_path);
    let any_binary = left_binary || middle_binary || right_binary;

    let left_buf = create_source_buffer(left_path, &s);
    let middle_buf = create_source_buffer(middle_path, &s);
    let right_buf = create_source_buffer(right_path, &s);
    left_buf.set_text(&left_content);
    middle_buf.set_text(&middle_content);
    right_buf.set_text(&right_content);
    left_buf.place_cursor(&left_buf.start_iter());
    middle_buf.place_cursor(&middle_buf.start_iter());
    right_buf.place_cursor(&right_buf.start_iter());

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
        middle_path,
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

    // Left/right panes are read-only in merge mode (only middle is editable)
    left_pane.text_view.set_editable(false);
    left_pane.save_btn.set_visible(false);
    right_pane.text_view.set_editable(false);
    right_pane.save_btn.set_visible(false);

    if any_binary {
        middle_pane.text_view.set_editable(false);
        middle_pane.save_btn.set_visible(false);
        for pane in [&left_pane, &middle_pane, &right_pane] {
            let bar = make_info_bar("Binary file — cannot display diff");
            pane.container
                .insert_child_after(&bar, pane.container.first_child().as_ref());
        }
    }

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
        let och = right_chunks.clone();
        left_gutter.set_draw_func(move |area, cr, width, _height| {
            draw_merge_gutter(
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
                &och.borrow(),
                &GutterArrows::LeftToRight,
                Side::A,
            );
        });
    }

    // Left gutter click: → copies left→middle only (left is read-only)
    {
        let gesture = GestureClick::new();
        let ltv = left_pane.text_view.clone();
        let lb = left_buf.clone();
        let mb = middle_buf.clone();
        let ls = left_pane.scroll.clone();
        let ch = left_chunks.clone();
        let och = right_chunks.clone();
        let g = left_gutter.clone();
        gesture.connect_pressed(move |_, _, _x, y| {
            let merged = merged_gutter_chunks(&ch.borrow(), &och.borrow(), Side::A);
            for (chunk, _) in &merged {
                if chunk.tag == DiffTag::Equal {
                    continue;
                }
                let lt = line_to_gutter_y(&ltv, &lb, chunk.start_a, &ls, &g);
                let lbot = line_to_gutter_y(&ltv, &lb, chunk.end_a, &ls, &g);
                let mid = f64::midpoint(lt, lbot);
                if (y - mid).abs() < 12.0 {
                    copy_chunk(
                        &lb,
                        chunk.start_a,
                        chunk.end_a,
                        &mb,
                        chunk.start_b,
                        chunk.end_b,
                    );
                    return;
                }
            }
        });
        left_gutter.add_controller(gesture);
    }

    // Left gutter right-click context menu
    {
        let lg_pending: Rc<RefCell<Option<DiffChunk>>> = Rc::new(RefCell::new(None));
        let lg_ctx = gio::SimpleActionGroup::new();
        {
            let action = gio::SimpleAction::new("copy-left-middle", None);
            let pc = lg_pending.clone();
            let lb = left_buf.clone();
            let mb = middle_buf.clone();
            action.connect_activate(move |_, _| {
                if let Some(c) = *pc.borrow() {
                    copy_chunk(&lb, c.start_a, c.end_a, &mb, c.start_b, c.end_b);
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
        let och = right_chunks.clone();
        let g = left_gutter.clone();
        let pc = lg_pending;
        let pop = lg_popover;
        gesture.connect_pressed(move |_, _, x, y| {
            let merged = merged_gutter_chunks(&ch.borrow(), &och.borrow(), Side::A);
            for (chunk, _) in &merged {
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
                    *pc.borrow_mut() = Some(*chunk);
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
        let och = left_chunks.clone();
        right_gutter.set_draw_func(move |area, cr, width, _height| {
            draw_merge_gutter(
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
                &och.borrow(),
                &GutterArrows::RightToLeft,
                Side::B,
            );
        });
    }

    // Right gutter click: ← copies right→middle only (right is read-only)
    {
        let gesture = GestureClick::new();
        let rtv = right_pane.text_view.clone();
        let mb = middle_buf.clone();
        let rb = right_buf.clone();
        let rs = right_pane.scroll.clone();
        let ch = right_chunks.clone();
        let och = left_chunks.clone();
        let g = right_gutter.clone();
        gesture.connect_pressed(move |_, _, _x, y| {
            let merged = merged_gutter_chunks(&ch.borrow(), &och.borrow(), Side::B);
            for (chunk, _) in &merged {
                if chunk.tag == DiffTag::Equal {
                    continue;
                }
                let rt = line_to_gutter_y(&rtv, &rb, chunk.start_b, &rs, &g);
                let rbot = line_to_gutter_y(&rtv, &rb, chunk.end_b, &rs, &g);
                let mid = f64::midpoint(rt, rbot);
                if (y - mid).abs() < 12.0 {
                    copy_chunk(
                        &rb,
                        chunk.start_b,
                        chunk.end_b,
                        &mb,
                        chunk.start_a,
                        chunk.end_a,
                    );
                    return;
                }
            }
        });
        right_gutter.add_controller(gesture);
    }

    // Right gutter right-click context menu
    {
        let rg_pending: Rc<RefCell<Option<DiffChunk>>> = Rc::new(RefCell::new(None));
        let rg_ctx = gio::SimpleActionGroup::new();
        {
            let action = gio::SimpleAction::new("copy-right-middle", None);
            let pc = rg_pending.clone();
            let mb = middle_buf.clone();
            let rb = right_buf.clone();
            action.connect_activate(move |_, _| {
                if let Some(c) = *pc.borrow() {
                    copy_chunk(&rb, c.start_b, c.end_b, &mb, c.start_a, c.end_a);
                }
            });
            rg_ctx.add_action(&action);
        }
        right_gutter.insert_action_group("rgutter", Some(&rg_ctx));

        let rg_menu = gio::Menu::new();
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
        let och = left_chunks.clone();
        let g = right_gutter.clone();
        let pc = rg_pending;
        let pop = rg_popover;
        gesture.connect_pressed(move |_, _, x, y| {
            let merged = merged_gutter_chunks(&ch.borrow(), &och.borrow(), Side::B);
            for (chunk, _) in &merged {
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
                    *pc.borrow_mut() = Some(*chunk);
                    pop.set_pointing_to(Some(&gtk4::gdk::Rectangle::new(x as i32, y as i32, 1, 1)));
                    pop.popup();
                    return;
                }
            }
        });
        right_gutter.add_controller(gesture);
    }

    // Text filter state
    let ignore_blanks: Rc<Cell<bool>> = Rc::new(Cell::new(settings.borrow().ignore_blank_lines));
    let ignore_whitespace: Rc<Cell<bool>> = Rc::new(Cell::new(settings.borrow().ignore_whitespace));

    // Scroll sync
    setup_scroll_sync_3way(
        &left_pane.scroll,
        &middle_pane.scroll,
        &right_pane.scroll,
        &left_pane.text_view,
        &middle_pane.text_view,
        &right_pane.text_view,
        &left_buf,
        &middle_buf,
        &right_buf,
        &left_chunks,
        &right_chunks,
        &left_gutter,
        &right_gutter,
    );

    // ── Toolbar with chunk navigation ───────────────────────────
    let current_chunk: Rc<Cell<Option<(usize, bool)>>> = Rc::new(Cell::new(None));

    // ── Filler overlay drawing (3-way) ──────────────────────────
    {
        let ltv = left_pane.text_view.clone();
        let ls = left_pane.scroll.clone();
        let lch = left_chunks.clone();
        let rch = right_chunks.clone();
        let cur = current_chunk.clone();
        left_pane
            .filler_overlay
            .set_draw_func(move |_area, cr, w, _h| {
                let w = w as f64;
                // Left pane shows a-side of left_chunks
                let cur_left = cur.get().and_then(|(i, r)| if r { None } else { Some(i) });
                draw_chunk_backgrounds(cr, w, &ltv, &ls, &lch.borrow(), Side::A, cur_left);
                draw_side_conflict_strokes(cr, w, &ltv, &ls, &lch.borrow(), &rch.borrow(), Side::A);
                draw_merge_fillers(
                    cr,
                    w,
                    &ltv,
                    &ls,
                    &lch.borrow(),
                    true,
                    &lch.borrow(),
                    &rch.borrow(),
                );
                draw_merge_fillers(
                    cr,
                    w,
                    &ltv,
                    &ls,
                    &rch.borrow(),
                    true,
                    &lch.borrow(),
                    &rch.borrow(),
                );
            });
    }
    {
        let rtv = right_pane.text_view.clone();
        let rs = right_pane.scroll.clone();
        let lch = left_chunks.clone();
        let rch = right_chunks.clone();
        let cur = current_chunk.clone();
        right_pane
            .filler_overlay
            .set_draw_func(move |_area, cr, w, _h| {
                let w = w as f64;
                // Right pane shows b-side of right_chunks
                let cur_right = cur.get().and_then(|(i, r)| if r { Some(i) } else { None });
                draw_chunk_backgrounds(cr, w, &rtv, &rs, &rch.borrow(), Side::B, cur_right);
                draw_side_conflict_strokes(cr, w, &rtv, &rs, &lch.borrow(), &rch.borrow(), Side::B);
                draw_merge_fillers(
                    cr,
                    w,
                    &rtv,
                    &rs,
                    &rch.borrow(),
                    false,
                    &lch.borrow(),
                    &rch.borrow(),
                );
                draw_merge_fillers(
                    cr,
                    w,
                    &rtv,
                    &rs,
                    &lch.borrow(),
                    false,
                    &lch.borrow(),
                    &rch.borrow(),
                );
            });
    }
    {
        let mtv = middle_pane.text_view.clone();
        let ms = middle_pane.scroll.clone();
        let lch = left_chunks.clone();
        let rch = right_chunks.clone();
        let cur = current_chunk.clone();
        middle_pane
            .filler_overlay
            .set_draw_func(move |_area, cr, w, _h| {
                let w = w as f64;
                // Middle pane: b-side of left_chunks, a-side of right_chunks
                let cur_val = cur.get();
                let cur_left = cur_val.and_then(|(i, r)| if r { None } else { Some(i) });
                let cur_right = cur_val.and_then(|(i, r)| if r { Some(i) } else { None });
                draw_chunk_backgrounds(cr, w, &mtv, &ms, &lch.borrow(), Side::B, cur_left);
                draw_chunk_backgrounds(cr, w, &mtv, &ms, &rch.borrow(), Side::A, cur_right);
                draw_conflict_backgrounds(cr, w, &mtv, &ms, &lch.borrow(), &rch.borrow());
                draw_merge_fillers(
                    cr,
                    w,
                    &mtv,
                    &ms,
                    &lch.borrow(),
                    false,
                    &lch.borrow(),
                    &rch.borrow(),
                );
                draw_merge_fillers(
                    cr,
                    w,
                    &mtv,
                    &ms,
                    &rch.borrow(),
                    true,
                    &lch.borrow(),
                    &rch.borrow(),
                );
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

    let (prev_btn, next_btn, nav_box) = build_nav_button_group(
        &format!("Previous change (Alt+Up / {}+E)", primary_key_name()),
        &format!("Next change (Alt+Down / {}+D)", primary_key_name()),
    );
    prev_btn.set_sensitive(false);
    next_btn.set_sensitive(false);

    // Conflict navigation buttons
    let (prev_conflict_btn, next_conflict_btn, conflict_nav_box) = build_nav_button_group(
        &format!("Previous conflict ({}+J)", primary_key_name()),
        &format!("Next conflict ({}+K)", primary_key_name()),
    );
    prev_conflict_btn.set_sensitive(false);
    next_conflict_btn.set_sensitive(false);

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

    let (_undo_btn, _redo_btn, undo_redo_box) = build_undo_redo_box(&active_view);

    // ── Find bar (shared helper) ──────────────────────────────────
    let action_group = gio::SimpleActionGroup::new();
    let find = build_find_bar(
        &action_group,
        &active_view,
        &middle_pane.scroll,
        &[left_buf.clone(), middle_buf.clone(), right_buf.clone()],
    );
    let find_revealer = find.revealer;
    let goto_entry = find.goto_entry;

    let toolbar = GtkBox::new(Orientation::Horizontal, 8);
    toolbar.set_margin_start(6);
    toolbar.set_margin_end(6);
    toolbar.set_margin_top(4);
    toolbar.set_margin_bottom(4);
    // Text filter toggles
    let (blank_toggle, ws_toggle, filter_box) =
        build_filter_toggles(ignore_blanks.get(), ignore_whitespace.get());

    let merge_prefs_btn = Button::from_icon_name("preferences-system-symbolic");
    merge_prefs_btn.set_tooltip_text(Some(&format!("Preferences ({}+,)", primary_key_name())));
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
    #[allow(clippy::too_many_arguments)]
    let navigate_merge_chunk = |lch: &[DiffChunk],
                                rch: &[DiffChunk],
                                cur: &Rc<Cell<Option<(usize, bool)>>>,
                                direction: i32,
                                ltv: &TextView,
                                lb: &TextBuffer,
                                l_scroll: &ScrolledWindow,
                                mtv: &TextView,
                                mb: &TextBuffer,
                                ms: &ScrolledWindow,
                                rtv: &TextView,
                                rb: &TextBuffer,
                                r_scroll: &ScrolledWindow,
                                lf: &DrawingArea,
                                mf: &DrawingArea,
                                rf: &DrawingArea,
                                active: &TextView,
                                wrap: bool| {
        // Search the active pane's own chunks in its own coordinates.
        let cl = cursor_line_from_view(active);
        let found: Option<(usize, bool)> = if active == ltv {
            let ne: Vec<usize> = lch
                .iter()
                .enumerate()
                .filter(|(_, c)| c.tag != DiffTag::Equal)
                .map(|(i, _)| i)
                .collect();
            let next = if direction > 0 {
                ne.iter()
                    .find(|&&i| lch[i].start_a > cl)
                    .or(if wrap { ne.first() } else { None })
            } else {
                ne.iter().rev().find(|&&i| lch[i].start_a < cl).or(if wrap {
                    ne.last()
                } else {
                    None
                })
            };
            next.map(|&idx| (idx, false))
        } else if active == rtv {
            let ne: Vec<usize> = rch
                .iter()
                .enumerate()
                .filter(|(_, c)| c.tag != DiffTag::Equal)
                .map(|(i, _)| i)
                .collect();
            let next = if direction > 0 {
                ne.iter()
                    .find(|&&i| rch[i].start_b > cl)
                    .or(if wrap { ne.first() } else { None })
            } else {
                ne.iter().rev().find(|&&i| rch[i].start_b < cl).or(if wrap {
                    ne.last()
                } else {
                    None
                })
            };
            next.map(|&idx| (idx, true))
        } else {
            let all = merge_change_indices(lch, rch);
            let middle_line = |&(idx, is_right): &(usize, bool)| -> usize {
                if is_right {
                    rch[idx].start_a
                } else {
                    lch[idx].start_b
                }
            };
            if direction > 0 {
                all.iter()
                    .find(|e| middle_line(e) > cl)
                    .or(if wrap { all.first() } else { None })
                    .copied()
            } else {
                all.iter()
                    .rev()
                    .find(|e| middle_line(e) < cl)
                    .or(if wrap { all.last() } else { None })
                    .copied()
            }
        };
        if let Some((idx, is_right)) = found {
            if is_right {
                // right_chunks: A = middle, B = right
                let chunk = &rch[idx];
                scroll_to_line(mtv, mb, chunk.start_a, ms);
                scroll_to_line(rtv, rb, chunk.start_b, r_scroll);
                place_cursor_at_line(mb, chunk.start_a);
                place_cursor_at_line(rb, chunk.start_b);
                // Left pane: no corresponding line — just scroll to middle line
                scroll_to_line(ltv, lb, chunk.start_a, l_scroll);
            } else {
                // left_chunks: A = left, B = middle
                let chunk = &lch[idx];
                scroll_to_line(mtv, mb, chunk.start_b, ms);
                scroll_to_line(ltv, lb, chunk.start_a, l_scroll);
                place_cursor_at_line(mb, chunk.start_b);
                place_cursor_at_line(lb, chunk.start_a);
                // Right pane: no corresponding line — just scroll to middle line
                scroll_to_line(rtv, rb, chunk.start_b, r_scroll);
            }
            // Set current_chunk AFTER placing cursors, because cursor_position_notify
            // handlers may overwrite it (e.g. for Insert chunks with zero-length ranges).
            cur.set(Some((idx, is_right)));
            lf.queue_draw();
            mf.queue_draw();
            rf.queue_draw();
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

    // Sensitivity helper for merge chunk nav buttons.
    // Determines whether prev/next have a valid target given the active pane.
    #[allow(clippy::too_many_arguments)]
    let merge_nav_sensitivity = |pb: &Button,
                                 nb: &Button,
                                 lch: &[DiffChunk],
                                 rch: &[DiffChunk],
                                 active: &TextView,
                                 ltv: &TextView,
                                 rtv: &TextView,
                                 wrap: bool| {
        let cl = cursor_line_from_view(active);
        let has_any = lch.iter().any(|c| c.tag != DiffTag::Equal)
            || rch.iter().any(|c| c.tag != DiffTag::Equal);
        if !has_any {
            pb.set_sensitive(false);
            nb.set_sensitive(false);
            return;
        }
        if wrap {
            pb.set_sensitive(true);
            nb.set_sensitive(true);
            return;
        }
        // Probe forward / backward in the same way navigate_merge_chunk does.
        if active == ltv {
            let (p, n) = diff_state::chunk_nav_sensitivity(lch, cl, Side::A, false);
            pb.set_sensitive(p);
            nb.set_sensitive(n);
        } else if active == rtv {
            let (p, n) = diff_state::chunk_nav_sensitivity(rch, cl, Side::B, false);
            pb.set_sensitive(p);
            nb.set_sensitive(n);
        } else {
            // Middle pane — uses merge_change_indices
            let all = merge_change_indices(lch, rch);
            let middle_line = |&(idx, is_right): &(usize, bool)| -> usize {
                if is_right {
                    rch[idx].start_a
                } else {
                    lch[idx].start_b
                }
            };
            let has_prev = all.iter().rev().any(|e| middle_line(e) < cl);
            let has_next = all.iter().any(|e| middle_line(e) > cl);
            pb.set_sensitive(has_prev);
            nb.set_sensitive(has_next);
        }
    };

    // Prev chunk
    {
        let lch = left_chunks.clone();
        let rch = right_chunks.clone();
        let cur = current_chunk.clone();
        let ltv = left_pane.text_view.clone();
        let lb = left_buf.clone();
        let l_scroll = left_pane.scroll.clone();
        let mtv = middle_pane.text_view.clone();
        let mb = middle_buf.clone();
        let ms = middle_pane.scroll.clone();
        let rtv = right_pane.text_view.clone();
        let rb = right_buf.clone();
        let r_scroll = right_pane.scroll.clone();
        let lbl = chunk_label.clone();
        let lf = left_pane.filler_overlay.clone();
        let mf = middle_pane.filler_overlay.clone();
        let rf = right_pane.filler_overlay.clone();
        let nav = navigate_merge_chunk;
        let upd = update_merge_label;
        let sens = merge_nav_sensitivity;
        let av = active_view.clone();
        let st = settings.clone();
        let pb = prev_btn.clone();
        let nb = next_btn.clone();
        prev_btn.connect_clicked(move |_| {
            nav(
                &lch.borrow(),
                &rch.borrow(),
                &cur,
                -1,
                &ltv,
                &lb,
                &l_scroll,
                &mtv,
                &mb,
                &ms,
                &rtv,
                &rb,
                &r_scroll,
                &lf,
                &mf,
                &rf,
                &av.borrow(),
                st.borrow().wrap_around_navigation,
            );
            upd(&lbl, &lch.borrow(), &rch.borrow(), cur.get());
            sens(
                &pb,
                &nb,
                &lch.borrow(),
                &rch.borrow(),
                &av.borrow(),
                &ltv,
                &rtv,
                st.borrow().wrap_around_navigation,
            );
            let focused_tv = av.borrow().clone();
            focused_tv.grab_focus();
        });
    }

    // Next chunk
    {
        let lch = left_chunks.clone();
        let rch = right_chunks.clone();
        let cur = current_chunk.clone();
        let ltv = left_pane.text_view.clone();
        let lb = left_buf.clone();
        let l_scroll = left_pane.scroll.clone();
        let mtv = middle_pane.text_view.clone();
        let mb = middle_buf.clone();
        let ms = middle_pane.scroll.clone();
        let rtv = right_pane.text_view.clone();
        let rb = right_buf.clone();
        let r_scroll = right_pane.scroll.clone();
        let lbl = chunk_label.clone();
        let lf = left_pane.filler_overlay.clone();
        let mf = middle_pane.filler_overlay.clone();
        let rf = right_pane.filler_overlay.clone();
        let nav = navigate_merge_chunk;
        let upd = update_merge_label;
        let sens = merge_nav_sensitivity;
        let av = active_view.clone();
        let st = settings.clone();
        let pb = prev_btn.clone();
        let nb = next_btn.clone();
        next_btn.connect_clicked(move |_| {
            nav(
                &lch.borrow(),
                &rch.borrow(),
                &cur,
                1,
                &ltv,
                &lb,
                &l_scroll,
                &mtv,
                &mb,
                &ms,
                &rtv,
                &rb,
                &r_scroll,
                &lf,
                &mf,
                &rf,
                &av.borrow(),
                st.borrow().wrap_around_navigation,
            );
            upd(&lbl, &lch.borrow(), &rch.borrow(), cur.get());
            sens(
                &pb,
                &nb,
                &lch.borrow(),
                &rch.borrow(),
                &av.borrow(),
                &ltv,
                &rtv,
                st.borrow().wrap_around_navigation,
            );
            let focused_tv = av.borrow().clone();
            focused_tv.grab_focus();
        });
    }

    // Navigate conflict helper — jumps between `<<<<<<<` markers in middle buf,
    // and syncs left/right panes to the corresponding line.
    #[allow(clippy::too_many_arguments)]
    let navigate_conflict = |cur: &Rc<Cell<Option<usize>>>,
                             direction: i32,
                             mtv: &TextView,
                             mb: &TextBuffer,
                             ms: &ScrolledWindow,
                             ltv: &TextView,
                             lb: &TextBuffer,
                             ls: &ScrolledWindow,
                             rtv: &TextView,
                             rb: &TextBuffer,
                             rs: &ScrolledWindow,
                             lch: &Rc<RefCell<Vec<DiffChunk>>>,
                             rch: &Rc<RefCell<Vec<DiffChunk>>>,
                             wrap: bool| {
        let cursor_line = cursor_line_from_view(mtv);
        let markers = find_conflict_markers(mb);
        if markers.is_empty() {
            return;
        }
        let next = if direction > 0 {
            markers
                .iter()
                .find(|&&l| l > cursor_line)
                .or(if wrap { markers.first() } else { None })
        } else {
            markers
                .iter()
                .rev()
                .find(|&&l| l < cursor_line)
                .or(if wrap { markers.last() } else { None })
        };
        if let Some(&line) = next {
            cur.set(Some(line));
            scroll_to_line(mtv, mb, line, ms);
            place_cursor_at_line(mb, line);
            // Sync left pane: left_chunks maps middle(start_b..end_b) → left(start_a)
            let left_line = lch
                .borrow()
                .iter()
                .find(|c| c.tag != DiffTag::Equal && c.start_b <= line && line < c.end_b)
                .map_or(line, |c| c.start_a);
            scroll_to_line(ltv, lb, left_line, ls);
            place_cursor_at_line(lb, left_line);
            // Sync right pane: right_chunks maps middle(start_a..end_a) → right(start_b)
            let right_line = rch
                .borrow()
                .iter()
                .find(|c| c.tag != DiffTag::Equal && c.start_a <= line && line < c.end_a)
                .map_or(line, |c| c.start_b);
            scroll_to_line(rtv, rb, right_line, rs);
            place_cursor_at_line(rb, right_line);
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

    // Sensitivity helper for conflict nav buttons.
    let conflict_nav_sensitivity =
        |pb: &Button, nb: &Button, mb: &TextBuffer, mtv: &TextView, wrap: bool| {
            let markers = find_conflict_markers(mb);
            if markers.is_empty() {
                pb.set_sensitive(false);
                nb.set_sensitive(false);
                return;
            }
            if wrap {
                pb.set_sensitive(true);
                nb.set_sensitive(true);
                return;
            }
            let cl = cursor_line_from_view(mtv);
            let has_prev = markers.iter().rev().any(|&l| l < cl);
            let has_next = markers.iter().any(|&l| l > cl);
            pb.set_sensitive(has_prev);
            nb.set_sensitive(has_next);
        };

    // Prev conflict
    {
        let cur = current_conflict.clone();
        let mtv = middle_pane.text_view.clone();
        let mb = middle_buf.clone();
        let ms = middle_pane.scroll.clone();
        let ltv = left_pane.text_view.clone();
        let lb = left_buf.clone();
        let ls = left_pane.scroll.clone();
        let rtv = right_pane.text_view.clone();
        let rb = right_buf.clone();
        let rs = right_pane.scroll.clone();
        let lch = left_chunks.clone();
        let rch = right_chunks.clone();
        let lbl = conflict_label.clone();
        let nav = navigate_conflict;
        let upd = update_conflict_label;
        let csens = conflict_nav_sensitivity;
        let av = active_view.clone();
        let st = settings.clone();
        let pcb = prev_conflict_btn.clone();
        let ncb = next_conflict_btn.clone();
        prev_conflict_btn.connect_clicked(move |_| {
            nav(
                &cur,
                -1,
                &mtv,
                &mb,
                &ms,
                &ltv,
                &lb,
                &ls,
                &rtv,
                &rb,
                &rs,
                &lch,
                &rch,
                st.borrow().wrap_around_navigation,
            );
            upd(&lbl, &mb, cur.get());
            csens(&pcb, &ncb, &mb, &mtv, st.borrow().wrap_around_navigation);
            let focused_tv = av.borrow().clone();
            focused_tv.grab_focus();
        });
    }

    // Next conflict
    {
        let cur = current_conflict.clone();
        let mtv = middle_pane.text_view.clone();
        let mb = middle_buf.clone();
        let ms = middle_pane.scroll.clone();
        let ltv = left_pane.text_view.clone();
        let lb = left_buf.clone();
        let ls = left_pane.scroll.clone();
        let rtv = right_pane.text_view.clone();
        let rb = right_buf.clone();
        let rs = right_pane.scroll.clone();
        let lch = left_chunks.clone();
        let rch = right_chunks.clone();
        let lbl = conflict_label.clone();
        let nav = navigate_conflict;
        let upd = update_conflict_label;
        let csens = conflict_nav_sensitivity;
        let av = active_view.clone();
        let st = settings.clone();
        let pcb = prev_conflict_btn.clone();
        let ncb = next_conflict_btn.clone();
        next_conflict_btn.connect_clicked(move |_| {
            nav(
                &cur,
                1,
                &mtv,
                &mb,
                &ms,
                &ltv,
                &lb,
                &ls,
                &rtv,
                &rb,
                &rs,
                &lch,
                &rch,
                st.borrow().wrap_around_navigation,
            );
            upd(&lbl, &mb, cur.get());
            csens(&pcb, &ncb, &mb, &mtv, st.borrow().wrap_around_navigation);
            let focused_tv = av.borrow().clone();
            focused_tv.grab_focus();
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
            draw_chunk_map(cr, h as f64, lb.line_count(), &ls, &ch.borrow(), true);
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
            let pb = prev_btn.clone();
            let nb = next_btn.clone();
            let pcb = prev_conflict_btn.clone();
            let ncb = next_conflict_btn.clone();
            let av = active_view.clone();
            let ltv = left_pane.text_view.clone();
            let rtv = right_pane.text_view.clone();
            let mtv = middle_pane.text_view.clone();
            let st = settings.clone();
            let lb = left_buf.clone();
            let rb = right_buf.clone();
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
                let pb = pb.clone();
                let nb = nb.clone();
                let pcb = pcb.clone();
                let ncb = ncb.clone();
                let av = av.clone();
                let ltv = ltv.clone();
                let rtv = rtv.clone();
                let mtv = mtv.clone();
                let st = st.clone();
                let lb = lb.clone();
                let rb = rb.clone();
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
                    merge_nav_sensitivity(
                        &pb,
                        &nb,
                        &lch.borrow(),
                        &rch.borrow(),
                        &av.borrow(),
                        &ltv,
                        &rtv,
                        st.borrow().wrap_around_navigation,
                    );
                    // Apply chunk background tags (paragraph_background) for all three panes.
                    // Side panes: insert/replace first, then override conflicts.
                    apply_chunk_bg_tags(&lb, &lch.borrow(), Side::A);
                    apply_side_conflict_bg_tags(&lb, &lch.borrow(), &rch.borrow(), Side::A);
                    apply_chunk_bg_tags(&mb, &lch.borrow(), Side::B);
                    apply_chunk_bg_tags(&mb, &rch.borrow(), Side::A);
                    apply_conflict_bg_tags(&mb, &lch.borrow(), &rch.borrow());
                    apply_chunk_bg_tags(&rb, &rch.borrow(), Side::B);
                    apply_side_conflict_bg_tags(&rb, &lch.borrow(), &rch.borrow(), Side::B);
                    ccur.set(None);
                    let n = find_conflict_markers(&mb).len();
                    if n == 0 {
                        clbl.set_label("No conflicts");
                    } else {
                        clbl.set_label(&format!("{n} conflicts"));
                    }
                    conflict_nav_sensitivity(
                        &pcb,
                        &ncb,
                        &mb,
                        &mtv,
                        st.borrow().wrap_around_navigation,
                    );
                }
            }
        };

        let pending = Rc::new(Cell::new(false));
        let connect_refresh = |buf: &TextBuffer| {
            let lb = left_buf.clone();
            let mb = middle_buf.clone();
            let rb = right_buf.clone();
            let lch = left_chunks.clone();
            let rch = right_chunks.clone();
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
                    let lch = lch.clone();
                    let rch = rch.clone();
                    let p = p.clone();
                    let ib = ib.clone();
                    let iw = iw.clone();
                    let cb = make_cb();
                    gtk4::glib::idle_add_local_once(move || {
                        refresh_merge_diffs(&lb, &mb, &rb, &lch, &rch, cb, ib.get(), iw.get(), &p);
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
            let lch = left_chunks.clone();
            let rch = right_chunks.clone();
            let on_complete = make_on_complete();
            pending.set(true);
            let p = pending.clone();
            let ib = ignore_blanks.get();
            let iw = ignore_whitespace.get();
            gtk4::glib::spawn_future_local(async move {
                let (lt_cmp, lt_map) = filter_for_diff(&left_content, iw, ib);
                let (mt_cmp, mt_map) = filter_for_diff(&middle_content, iw, ib);
                let (rt_cmp, rt_map) = filter_for_diff(&right_content, iw, ib);
                let lt_total = left_content.lines().count();
                let mt_total = middle_content.lines().count();
                let rt_total = right_content.lines().count();
                let left_ident = lt_cmp == mt_cmp;
                let right_ident = mt_cmp == rt_cmp;
                let (new_left_raw, new_right_raw) = gio::spawn_blocking(move || {
                    let nl = if left_ident {
                        Vec::new()
                    } else {
                        myers::diff_lines(&lt_cmp, &mt_cmp)
                    };
                    let nr = if right_ident {
                        Vec::new()
                    } else {
                        myers::diff_lines(&mt_cmp, &rt_cmp)
                    };
                    (nl, nr)
                })
                .await
                .unwrap_or_default();

                let new_left = remap_chunks(new_left_raw, &lt_map, lt_total, &mt_map, mt_total);
                let new_right = remap_chunks(new_right_raw, &mt_map, mt_total, &rt_map, rt_total);

                apply_merge_tags(&lb, &mb, &rb, &new_left, &new_right);
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
            let lch = left_chunks.clone();
            let rch = right_chunks.clone();
            let make_cb = make_on_complete.clone();
            let dummy = Rc::new(Cell::new(true));
            blank_toggle.connect_toggled(move |btn| {
                ib.set(btn.is_active());
                dummy.set(true);
                refresh_merge_diffs(
                    &lb,
                    &mb,
                    &rb,
                    &lch,
                    &rch,
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
            let lch = left_chunks.clone();
            let rch = right_chunks.clone();
            let dummy = Rc::new(Cell::new(true));
            ws_toggle.connect_toggled(move |btn| {
                iw.set(btn.is_active());
                dummy.set(true);
                refresh_merge_diffs(
                    &lb,
                    &mb,
                    &rb,
                    &lch,
                    &rch,
                    make_on_complete(),
                    ib.get(),
                    iw.get(),
                    &dummy,
                );
            });
        }
    }

    // ── Update chunk/conflict labels + nav sensitivity when cursor moves ───
    if !any_binary {
        // Left and right pane cursor tracking (chunk label only)
        let connect_side_cursor = |buf: &TextBuffer, tv: &TextView, side: Side, is_right: bool| {
            let lch = left_chunks.clone();
            let rch = right_chunks.clone();
            let cur = current_chunk.clone();
            let lbl = chunk_label.clone();
            let pb = prev_btn.clone();
            let nb = next_btn.clone();
            let av = active_view.clone();
            let ltv = left_pane.text_view.clone();
            let rtv = right_pane.text_view.clone();
            let st = settings.clone();
            let my_tv = tv.clone();
            let sens = merge_nav_sensitivity;
            buf.connect_cursor_position_notify(move |_| {
                if av.borrow().clone() != my_tv {
                    return;
                }
                let chunks = if is_right { rch.borrow() } else { lch.borrow() };
                let cursor_line = cursor_line_from_view(&my_tv);
                let at = diff_state::chunk_at_cursor(&chunks, cursor_line, side);
                cur.set(at.map(|idx| (idx, is_right)));
                let all = merge_change_indices(&lch.borrow(), &rch.borrow());
                let total = all.len();
                if total == 0 {
                    lbl.set_label("No changes");
                } else if let Some(cur_val) = cur.get() {
                    if let Some(pos) = all.iter().position(|v| *v == cur_val) {
                        lbl.set_label(&format!("Change {} of {}", pos + 1, total));
                    } else {
                        lbl.set_label(&format!("{total} changes"));
                    }
                } else {
                    lbl.set_label(&format!("{total} changes"));
                }
                let wrap = st.borrow().wrap_around_navigation;
                sens(
                    &pb,
                    &nb,
                    &lch.borrow(),
                    &rch.borrow(),
                    &my_tv,
                    &ltv,
                    &rtv,
                    wrap,
                );
            });
        };
        connect_side_cursor(&left_buf, &left_pane.text_view, Side::A, false);
        connect_side_cursor(&right_buf, &right_pane.text_view, Side::B, true);

        // Middle pane cursor tracking (both chunk and conflict labels)
        {
            let lch = left_chunks.clone();
            let rch = right_chunks.clone();
            let cur = current_chunk.clone();
            let ccur = current_conflict.clone();
            let lbl = chunk_label.clone();
            let clbl = conflict_label.clone();
            let pb = prev_btn.clone();
            let nb = next_btn.clone();
            let pcb = prev_conflict_btn.clone();
            let ncb = next_conflict_btn.clone();
            let av = active_view.clone();
            let ltv = left_pane.text_view.clone();
            let rtv = right_pane.text_view.clone();
            let mtv = middle_pane.text_view.clone();
            let mb = middle_buf.clone();
            let st = settings.clone();
            let sens = merge_nav_sensitivity;
            let csens = conflict_nav_sensitivity;
            middle_buf.connect_cursor_position_notify(move |_| {
                if av.borrow().clone() != mtv {
                    return;
                }
                let cursor_line = cursor_line_from_view(&mtv);

                // Chunk: check left_chunks (side B = middle) and right_chunks (side A = middle)
                let left_at = diff_state::chunk_at_cursor(&lch.borrow(), cursor_line, Side::B);
                let right_at = diff_state::chunk_at_cursor(&rch.borrow(), cursor_line, Side::A);
                let at = left_at
                    .map(|idx| (idx, false))
                    .or_else(|| right_at.map(|idx| (idx, true)));
                cur.set(at);
                update_merge_label(&lbl, &lch.borrow(), &rch.borrow(), at);

                let wrap = st.borrow().wrap_around_navigation;
                sens(
                    &pb,
                    &nb,
                    &lch.borrow(),
                    &rch.borrow(),
                    &mtv,
                    &ltv,
                    &rtv,
                    wrap,
                );

                // Conflict: check if cursor is inside a conflict block
                let in_conflict = conflict_at_cursor(&mb, cursor_line);
                ccur.set(in_conflict);
                update_conflict_label(&clbl, &mb, in_conflict);
                csens(&pcb, &ncb, &mb, &mtv, wrap);
            });
        }
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

    // Remaining GActions for keyboard shortcuts
    {
        let action = gio::SimpleAction::new("prev-chunk", None);
        let lch = left_chunks.clone();
        let rch = right_chunks.clone();
        let cur = current_chunk.clone();
        let ltv = left_pane.text_view.clone();
        let lb = left_buf.clone();
        let l_scroll = left_pane.scroll.clone();
        let mtv = middle_pane.text_view.clone();
        let mb = middle_buf.clone();
        let ms = middle_pane.scroll.clone();
        let rtv = right_pane.text_view.clone();
        let rb = right_buf.clone();
        let r_scroll = right_pane.scroll.clone();
        let lbl = chunk_label.clone();
        let lf = left_pane.filler_overlay.clone();
        let mf = middle_pane.filler_overlay.clone();
        let rf = right_pane.filler_overlay.clone();
        let av = active_view.clone();
        let st = settings.clone();
        let pb = prev_btn.clone();
        let nb = next_btn.clone();
        action.connect_activate(move |_, _| {
            navigate_merge_chunk(
                &lch.borrow(),
                &rch.borrow(),
                &cur,
                -1,
                &ltv,
                &lb,
                &l_scroll,
                &mtv,
                &mb,
                &ms,
                &rtv,
                &rb,
                &r_scroll,
                &lf,
                &mf,
                &rf,
                &av.borrow(),
                st.borrow().wrap_around_navigation,
            );
            update_merge_label(&lbl, &lch.borrow(), &rch.borrow(), cur.get());
            merge_nav_sensitivity(
                &pb,
                &nb,
                &lch.borrow(),
                &rch.borrow(),
                &av.borrow(),
                &ltv,
                &rtv,
                st.borrow().wrap_around_navigation,
            );
        });
        action_group.add_action(&action);
    }
    {
        let action = gio::SimpleAction::new("next-chunk", None);
        let lch = left_chunks.clone();
        let rch = right_chunks.clone();
        let cur = current_chunk.clone();
        let ltv = left_pane.text_view.clone();
        let lb = left_buf.clone();
        let l_scroll = left_pane.scroll.clone();
        let mtv = middle_pane.text_view.clone();
        let mb = middle_buf.clone();
        let ms = middle_pane.scroll.clone();
        let rtv = right_pane.text_view.clone();
        let rb = right_buf.clone();
        let r_scroll = right_pane.scroll.clone();
        let lbl = chunk_label.clone();
        let lf = left_pane.filler_overlay.clone();
        let mf = middle_pane.filler_overlay.clone();
        let rf = right_pane.filler_overlay.clone();
        let av = active_view.clone();
        let st = settings.clone();
        let pb = prev_btn.clone();
        let nb = next_btn.clone();
        action.connect_activate(move |_, _| {
            navigate_merge_chunk(
                &lch.borrow(),
                &rch.borrow(),
                &cur,
                1,
                &ltv,
                &lb,
                &l_scroll,
                &mtv,
                &mb,
                &ms,
                &rtv,
                &rb,
                &r_scroll,
                &lf,
                &mf,
                &rf,
                &av.borrow(),
                st.borrow().wrap_around_navigation,
            );
            update_merge_label(&lbl, &lch.borrow(), &rch.borrow(), cur.get());
            merge_nav_sensitivity(
                &pb,
                &nb,
                &lch.borrow(),
                &rch.borrow(),
                &av.borrow(),
                &ltv,
                &rtv,
                st.borrow().wrap_around_navigation,
            );
        });
        action_group.add_action(&action);
    }
    {
        let action = gio::SimpleAction::new("prev-conflict", None);
        let cur = current_conflict.clone();
        let mtv = middle_pane.text_view.clone();
        let mb = middle_buf.clone();
        let ms = middle_pane.scroll.clone();
        let ltv = left_pane.text_view.clone();
        let lb = left_buf.clone();
        let ls = left_pane.scroll.clone();
        let rtv = right_pane.text_view.clone();
        let rb = right_buf.clone();
        let rs = right_pane.scroll.clone();
        let lch = left_chunks.clone();
        let rch = right_chunks.clone();
        let lbl = conflict_label.clone();
        let st = settings.clone();
        let pcb = prev_conflict_btn.clone();
        let ncb = next_conflict_btn.clone();
        action.connect_activate(move |_, _| {
            navigate_conflict(
                &cur,
                -1,
                &mtv,
                &mb,
                &ms,
                &ltv,
                &lb,
                &ls,
                &rtv,
                &rb,
                &rs,
                &lch,
                &rch,
                st.borrow().wrap_around_navigation,
            );
            update_conflict_label(&lbl, &mb, cur.get());
            conflict_nav_sensitivity(&pcb, &ncb, &mb, &mtv, st.borrow().wrap_around_navigation);
        });
        action_group.add_action(&action);
    }
    {
        let action = gio::SimpleAction::new("next-conflict", None);
        let cur = current_conflict.clone();
        let mtv = middle_pane.text_view.clone();
        let mb = middle_buf.clone();
        let ms = middle_pane.scroll.clone();
        let ltv = left_pane.text_view.clone();
        let lb = left_buf.clone();
        let ls = left_pane.scroll.clone();
        let rtv = right_pane.text_view.clone();
        let rb = right_buf.clone();
        let rs = right_pane.scroll.clone();
        let lch = left_chunks.clone();
        let rch = right_chunks.clone();
        let lbl = conflict_label.clone();
        let st = settings.clone();
        let pcb = prev_conflict_btn.clone();
        let ncb = next_conflict_btn.clone();
        action.connect_activate(move |_, _| {
            navigate_conflict(
                &cur,
                1,
                &mtv,
                &mb,
                &ms,
                &ltv,
                &lb,
                &ls,
                &rtv,
                &rb,
                &rs,
                &lch,
                &rch,
                st.borrow().wrap_around_navigation,
            );
            update_conflict_label(&lbl, &mb, cur.get());
            conflict_nav_sensitivity(&pcb, &ncb, &mb, &mtv, st.borrow().wrap_around_navigation);
        });
        action_group.add_action(&action);
    }
    // Ctrl+S: save middle pane (only editable pane in merge)
    {
        let action = gio::SimpleAction::new("save", None);
        let mb = middle_buf.clone();
        let msp = middle_pane.save_path.clone();
        let ms = middle_pane.save_btn.clone();
        action.connect_activate(move |_, _| {
            if ms.is_sensitive() {
                let text = mb.text(&mb.start_iter(), &mb.end_iter(), false);
                save_file(&msp.borrow(), text.as_str(), &ms);
            }
        });
        action_group.add_action(&action);
    }

    // Ctrl+R / F5: refresh — re-read all 3 files from disk
    {
        let action = gio::SimpleAction::new("refresh", None);
        let lb = left_buf.clone();
        let mb = middle_buf.clone();
        let rb = right_buf.clone();
        let lsp = left_pane.save_path.clone();
        let msp = middle_pane.save_path.clone();
        let rsp = right_pane.save_path.clone();
        let ms = middle_pane.save_btn.clone();
        action.connect_activate(move |_, _| {
            refresh_panes(
                &ms,
                vec![
                    (lb.clone(), lsp.clone(), None),
                    (mb.clone(), msp.clone(), Some(ms.clone())),
                    (rb.clone(), rsp.clone(), None),
                ],
            );
        });
        action_group.add_action(&action);
    }

    // Ctrl+Shift+O: open focused file externally
    {
        let action = gio::SimpleAction::new("open-externally", None);
        let av = active_view.clone();
        let ltv = left_pane.text_view.clone();
        let mtv = middle_pane.text_view.clone();
        let lsp = left_pane.save_path.clone();
        let msp = middle_pane.save_path.clone();
        let rsp = right_pane.save_path.clone();
        action.connect_activate(move |_, _| {
            let active = av.borrow().clone();
            let path = if active == ltv {
                lsp.borrow().clone()
            } else if active == mtv {
                msp.borrow().clone()
            } else {
                rsp.borrow().clone()
            };
            open_externally(&path);
        });
        action_group.add_action(&action);
    }

    // Ctrl+Shift+S: save as — save middle pane to a new path
    {
        let action = gio::SimpleAction::new("save-as", None);
        let mb = middle_buf.clone();
        let msp = middle_pane.save_path.clone();
        let ms = middle_pane.save_btn.clone();
        let ml = middle_pane.path_label.clone();
        let mtp = middle_pane.tab_path.clone();
        action.connect_activate(move |_, _| {
            save_as_pane(
                mb.clone(),
                msp.clone(),
                ms.clone(),
                ml.clone(),
                Some(mtp.clone()),
            );
        });
        action_group.add_action(&action);
    }

    // Ctrl+Shift+L: save all dirty panes (middle only in merge)
    {
        let action = gio::SimpleAction::new("save-all", None);
        let mb = middle_buf.clone();
        let msp = middle_pane.save_path.clone();
        let ms = middle_pane.save_btn.clone();
        action.connect_activate(move |_, _| {
            save_all_panes(&[(mb.clone(), msp.clone(), ms.clone())]);
        });
        action_group.add_action(&action);
    }

    // Alt+Left: copy right chunk → middle (used from right pane, pushes content left)
    {
        let action = gio::SimpleAction::new("copy-chunk-right-middle", None);
        let lch = left_chunks.clone();
        let rch = right_chunks.clone();
        let mb = middle_buf.clone();
        let rb = right_buf.clone();
        let rtv = right_pane.text_view.clone();
        action.connect_activate(move |_, _| {
            let cursor_line = cursor_line_from_view(&rtv);
            let merged = merged_gutter_chunks(&rch.borrow(), &lch.borrow(), Side::B);
            for (mc, _) in &merged {
                if mc.tag == DiffTag::Equal {
                    continue;
                }
                // B-side = right pane range for right_chunks
                if cursor_line >= mc.start_b && cursor_line < mc.end_b.max(mc.start_b + 1) {
                    copy_chunk(&rb, mc.start_b, mc.end_b, &mb, mc.start_a, mc.end_a);
                    return;
                }
            }
        });
        action_group.add_action(&action);
    }
    // Alt+Right: copy left chunk → middle (used from left pane, pushes content right)
    {
        let action = gio::SimpleAction::new("copy-chunk-left-middle", None);
        let lch = left_chunks.clone();
        let rch = right_chunks.clone();
        let lb = left_buf.clone();
        let mb = middle_buf.clone();
        let ltv = left_pane.text_view.clone();
        action.connect_activate(move |_, _| {
            let cursor_line = cursor_line_from_view(&ltv);
            let merged = merged_gutter_chunks(&lch.borrow(), &rch.borrow(), Side::A);
            for (mc, _) in &merged {
                if mc.tag == DiffTag::Equal {
                    continue;
                }
                // A-side = left pane range for left_chunks
                if cursor_line >= mc.start_a && cursor_line < mc.end_a.max(mc.start_a + 1) {
                    copy_chunk(&lb, mc.start_a, mc.end_a, &mb, mc.start_b, mc.end_b);
                    return;
                }
            }
        });
        action_group.add_action(&action);
    }

    // Capture-phase key handler for shortcuts sourceview would consume
    for tv in [
        &left_pane.text_view,
        &middle_pane.text_view,
        &right_pane.text_view,
    ] {
        let key_ctl = EventControllerKey::new();
        key_ctl.set_propagation_phase(gtk4::PropagationPhase::Capture);
        let ag = action_group.clone();
        let fr = find_revealer.clone();
        let lb = left_buf.clone();
        let mb = middle_buf.clone();
        let rb = right_buf.clone();
        key_ctl.connect_key_pressed(move |_, key, _, mods| {
            if key == gtk4::gdk::Key::Escape && fr.is_child_revealed() {
                fr.set_reveal_child(false);
                clear_search_tags(&lb);
                clear_search_tags(&mb);
                clear_search_tags(&rb);
                return gtk4::glib::Propagation::Stop;
            }
            let action_name = if mods.contains(gtk4::gdk::ModifierType::ALT_MASK) {
                match key {
                    k if k == gtk4::gdk::Key::Up => Some("prev-chunk"),
                    k if k == gtk4::gdk::Key::Down => Some("next-chunk"),
                    k if k == gtk4::gdk::Key::Left => Some("copy-chunk-right-middle"),
                    k if k == gtk4::gdk::Key::Right => Some("copy-chunk-left-middle"),
                    _ => None,
                }
            } else if has_primary_modifier(mods) {
                if mods.contains(gtk4::gdk::ModifierType::SHIFT_MASK) {
                    if key == gtk4::gdk::Key::o || key == gtk4::gdk::Key::O {
                        Some("open-externally")
                    } else if key == gtk4::gdk::Key::s || key == gtk4::gdk::Key::S {
                        Some("save-as")
                    } else if key == gtk4::gdk::Key::l || key == gtk4::gdk::Key::L {
                        Some("save-all")
                    } else {
                        None
                    }
                } else if key == gtk4::gdk::Key::s || key == gtk4::gdk::Key::S {
                    Some("save")
                } else if key == gtk4::gdk::Key::r || key == gtk4::gdk::Key::R {
                    Some("refresh")
                } else if key == gtk4::gdk::Key::e || key == gtk4::gdk::Key::E {
                    Some("prev-chunk")
                } else if key == gtk4::gdk::Key::d || key == gtk4::gdk::Key::D {
                    Some("next-chunk")
                } else if key == gtk4::gdk::Key::f || key == gtk4::gdk::Key::F {
                    Some("find")
                } else if key == gtk4::gdk::Key::h || key == gtk4::gdk::Key::H {
                    Some("find-replace")
                } else if key == gtk4::gdk::Key::l || key == gtk4::gdk::Key::L {
                    Some("go-to-line")
                } else if key == gtk4::gdk::Key::j || key == gtk4::gdk::Key::J {
                    Some("prev-conflict")
                } else if key == gtk4::gdk::Key::k || key == gtk4::gdk::Key::K {
                    Some("next-conflict")
                } else {
                    None
                }
            } else if key == gtk4::gdk::Key::F3 {
                if mods.contains(gtk4::gdk::ModifierType::SHIFT_MASK) {
                    Some("find-prev")
                } else {
                    Some("find-next")
                }
            } else if key == gtk4::gdk::Key::F5 {
                Some("refresh")
            } else {
                None
            };
            if let Some(name) = action_name {
                if let Some(action) = ag.lookup_action(name) {
                    action
                        .downcast_ref::<gio::SimpleAction>()
                        .unwrap()
                        .activate(None);
                }
                return gtk4::glib::Propagation::Stop;
            }
            gtk4::glib::Propagation::Proceed
        });
        tv.add_controller(key_ctl);
    }

    MergeViewResult {
        widget,
        left_buf,
        middle_buf,
        right_buf,
        middle_save: middle_pane.save_btn,
        middle_tab_path: middle_pane.tab_path,
        action_group,
    }
}

pub(super) fn build_merge_window(
    app: &Application,
    left_path: std::path::PathBuf,
    middle_path: std::path::PathBuf,
    right_path: std::path::PathBuf,
    labels: &[String],
    settings: &Rc<RefCell<Settings>>,
) {
    let mv = build_merge_view(&left_path, &middle_path, &right_path, labels, settings);

    // File watcher on all 3 files
    let watch_paths: Vec<&Path> = [&left_path, &middle_path, &right_path]
        .iter()
        .filter_map(|p| p.parent())
        .collect();
    let lb = mv.left_buf.clone();
    let mb = mv.middle_buf.clone();
    let rb = mv.right_buf.clone();
    let lp = left_path.clone();
    let mp = middle_path.clone();
    let rp = right_path.clone();
    let m_save = mv.middle_save.clone();
    let loading = Rc::new(Cell::new(false));
    let dirty = Rc::new(Cell::new(false));
    let retry_count = Rc::new(Cell::new(0u32));
    let merge_watcher = start_file_watcher(&watch_paths, false, None, move |fs_dirty| {
        if fs_dirty {
            dirty.set(true);
            retry_count.set(0);
        }
        if dirty.get() && !loading.get() && !is_saving(&[&lp, &mp, &rp]) && !m_save.is_sensitive() {
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
            let retry2 = retry_count.clone();
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
                let (Some(left_content), Some(middle_content), Some(right_content)) =
                    (left_content, middle_content, right_content)
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
                let cur_l = lb2.text(&lb2.start_iter(), &lb2.end_iter(), false);
                let cur_m = mb2.text(&mb2.start_iter(), &mb2.end_iter(), false);
                let cur_r = rb2.text(&rb2.start_iter(), &rb2.end_iter(), false);
                if cur_l.as_str() != left_content
                    || cur_m.as_str() != middle_content
                    || cur_r.as_str() != right_content
                {
                    lb2.set_text(&left_content);
                    mb2.set_text(&middle_content);
                    rb2.set_text(&right_content);
                    m_save2.set_sensitive(false);
                }
            });
        }
    });

    // Window title / tab title
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

    // ── Window via shared helper ─────────────────────────────────────
    let AppWindow {
        window,
        notebook,
        open_tabs,
    } = build_app_window(app, settings, 1200, 600, true);

    mv.widget
        .insert_action_group("diff", Some(&mv.action_group));
    notebook.append_page(&mv.widget, Some(&Label::new(Some(&title))));

    // Register merge tab in open_tabs so the shared close_request handler
    // picks up the middle_save unsaved state automatically.
    {
        let tab_id = NEXT_TAB_ID.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        open_tabs.borrow_mut().push(FileTab::Merge {
            id: tab_id,
            rel_path: title,
            widget: mv.widget.clone(),
            middle: PaneInfo {
                path: mv.middle_tab_path,
                buf: mv.middle_buf.clone(),
                save: mv.middle_save,
            },
        });
    }

    window.connect_destroy(move |_| {
        merge_watcher.alive.set(false);
    });

    window.present();
}

// Tests for merge_change_indices and find_conflict_markers are in merge_state.rs
