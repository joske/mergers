#[allow(clippy::wildcard_imports)]
use super::*;

pub struct FindBar {
    pub revealer: Revealer,
    pub goto_entry: Entry,
}

/// Build the find/replace bar and goto-line entry, registering find/find-next/
/// find-prev/find-replace/go-to-line actions on `action_group`.
///
/// `panes` lists every (`TextView`, `TextBuffer`) pair so that find-next/prev
/// can cycle across panes when no more matches remain in the active one.
pub fn build_find_bar(
    action_group: &gio::SimpleActionGroup,
    active_view: &Rc<RefCell<TextView>>,
    fallback_scroll: &ScrolledWindow,
    panes: &[(TextView, TextBuffer)],
) -> FindBar {
    // ── Widget construction ─────────────────────────────────────────
    let find_entry = Entry::new();
    find_entry.set_placeholder_text(Some(&format!("Find ({}+F)", primary_key_name())));
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

    // ── Search logic: highlight matches in all buffers ───────────────
    let buffers: Vec<TextBuffer> = panes.iter().map(|(_, b)| b.clone()).collect();
    let all_panes: Vec<(TextView, TextBuffer)> = panes.to_vec();
    {
        let bufs = buffers.clone();
        let ml = match_label.clone();
        find_entry.connect_changed(move |e| {
            let needle = e.text().to_string();
            let total: usize = bufs
                .iter()
                .map(|b| highlight_search_matches(b, &needle))
                .sum();
            if needle.is_empty() {
                ml.set_label("");
            } else if total == 0 {
                ml.set_label("No matches");
            } else {
                ml.set_label(&format!("{total} matches"));
            }
        });
    }

    // ── Shared cross-pane find helper ─────────────────────────────────
    // Searches the active pane first; if no match, cycles to the next pane.
    let do_find = {
        let all = all_panes.clone();
        let av = active_view.clone();
        let fs = fallback_scroll.clone();
        move |needle: &str, forward: bool| {
            if needle.is_empty() {
                return;
            }
            let current_tv = av.borrow().clone();
            let current_buf = current_tv.buffer();

            // Try to find in the current buffer first
            let cursor = current_buf.iter_at_mark(&current_buf.get_insert());
            if let Some((start, end)) = find_next_match(&current_buf, needle, &cursor, forward) {
                current_buf.select_range(&start, &end);
                let scroll = scroll_for_view(&current_tv, &fs);
                scroll_to_line(&current_tv, &current_buf, start.line() as usize, &scroll);
                return;
            }

            // Not found in current pane — cycle through other panes
            let current_idx = all
                .iter()
                .position(|(tv, _)| *tv == current_tv)
                .unwrap_or(0);
            let n = all.len();
            for offset in 1..n {
                let idx = if forward {
                    (current_idx + offset) % n
                } else {
                    (current_idx + n - offset) % n
                };
                let (ref tv, ref buf) = all[idx];
                let from = if forward {
                    buf.start_iter()
                } else {
                    buf.end_iter()
                };
                if let Some((start, end)) = find_next_match(buf, needle, &from, forward) {
                    buf.select_range(&start, &end);
                    *av.borrow_mut() = tv.clone();
                    tv.grab_focus();
                    let scroll = scroll_for_view(tv, &fs);
                    scroll_to_line(tv, buf, start.line() as usize, &scroll);
                    return;
                }
            }
        }
    };

    // ── Find next button ─────────────────────────────────────────────
    {
        let fe = find_entry.clone();
        let find = do_find.clone();
        find_next_btn.connect_clicked(move |_| {
            find(&fe.text(), true);
        });
    }

    // ── Find prev button ─────────────────────────────────────────────
    {
        let fe = find_entry.clone();
        let find = do_find.clone();
        find_prev_btn.connect_clicked(move |_| {
            find(&fe.text(), false);
        });
    }

    // ── Enter in find entry = find next ──────────────────────────────
    {
        let find = do_find.clone();
        find_entry.connect_activate(move |e| {
            find(&e.text(), true);
        });
    }

    // ── Replace ──────────────────────────────────────────────────────
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

    // ── Replace all ──────────────────────────────────────────────────
    {
        let bufs = buffers.clone();
        let find_e = find_entry.clone();
        let repl_e = replace_entry.clone();
        replace_all_btn.connect_clicked(move |_| {
            let needle = find_e.text().to_string();
            let replacement = repl_e.text().to_string();
            if needle.is_empty() {
                return;
            }
            let needle_lower = needle.to_lowercase();
            for buf in &bufs {
                let text = buf
                    .text(&buf.start_iter(), &buf.end_iter(), false)
                    .to_string();
                // Find all match positions (byte offsets) in reverse order so
                // earlier replacements don't shift later offsets.
                let text_lower = text.to_lowercase();
                let mut positions: Vec<usize> = Vec::new();
                let mut search_from = 0;
                while let Some(pos) = text_lower[search_from..].find(&needle_lower) {
                    positions.push(search_from + pos);
                    search_from += pos + needle.len();
                }
                if positions.is_empty() {
                    continue;
                }
                buf.begin_user_action();
                for &byte_pos in positions.iter().rev() {
                    let char_start = text[..byte_pos].chars().count() as i32;
                    let char_end =
                        char_start + text[byte_pos..byte_pos + needle.len()].chars().count() as i32;
                    let mut s = buf.iter_at_offset(char_start);
                    let mut e = buf.iter_at_offset(char_end);
                    buf.delete(&mut s, &mut e);
                    buf.insert(&mut s, &replacement);
                }
                buf.end_user_action();
            }
        });
    }

    // ── Close find bar ───────────────────────────────────────────────
    {
        let fr = find_revealer.clone();
        let bufs = buffers.clone();
        find_close_btn.connect_clicked(move |_| {
            fr.set_reveal_child(false);
            for b in &bufs {
                clear_search_tags(b);
            }
        });
    }

    // ── Escape / F3 in find and replace entries ──────────────────────
    for entry in [&find_entry, &replace_entry] {
        let fr = find_revealer.clone();
        let bufs = buffers.clone();
        let fnb = find_next_btn.clone();
        let fpb = find_prev_btn.clone();
        let key_ctl = EventControllerKey::new();
        key_ctl.connect_key_pressed(move |_, key, _, mods| {
            if key == gtk4::gdk::Key::Escape {
                fr.set_reveal_child(false);
                for b in &bufs {
                    clear_search_tags(b);
                }
                return gtk4::glib::Propagation::Stop;
            }
            if key == gtk4::gdk::Key::F3 {
                if mods.contains(gtk4::gdk::ModifierType::SHIFT_MASK) {
                    fpb.emit_clicked();
                } else {
                    fnb.emit_clicked();
                }
                return gtk4::glib::Propagation::Stop;
            }
            gtk4::glib::Propagation::Proceed
        });
        entry.add_controller(key_ctl);
    }

    // ── Goto-line entry ──────────────────────────────────────────────
    let goto_entry = Entry::new();
    goto_entry.set_placeholder_text(Some("Line #"));
    goto_entry.set_width_chars(8);
    goto_entry.add_css_class("goto-entry");
    goto_entry.set_visible(false);
    {
        let av = active_view.clone();
        let fs = fallback_scroll.clone();
        let entry = goto_entry.clone();
        goto_entry.connect_activate(move |e| {
            if let Ok(line) = e.text().trim().parse::<usize>() {
                let tv = av.borrow().clone();
                let buf = tv.buffer();
                let target = line.saturating_sub(1);
                let scroll = scroll_for_view(&tv, &fs);
                scroll_to_line(&tv, &buf, target, &scroll);
                if let Some(iter) = buf.iter_at_line(target as i32) {
                    buf.place_cursor(&iter);
                }
                tv.grab_focus();
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

    // ── Register actions ─────────────────────────────────────────────
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
    {
        let action = gio::SimpleAction::new("find-next", None);
        let fe = find_entry.clone();
        let find = do_find.clone();
        action.connect_activate(move |_, _| {
            find(&fe.text(), true);
        });
        action_group.add_action(&action);
    }
    {
        let action = gio::SimpleAction::new("find-prev", None);
        let fe = find_entry.clone();
        let find = do_find.clone();
        action.connect_activate(move |_, _| {
            find(&fe.text(), false);
        });
        action_group.add_action(&action);
    }
    {
        let action = gio::SimpleAction::new("go-to-line", None);
        let ge = goto_entry.clone();
        action.connect_activate(move |_, _| {
            ge.set_visible(true);
            ge.grab_focus();
        });
        action_group.add_action(&action);
    }

    FindBar {
        revealer: find_revealer,
        goto_entry,
    }
}
