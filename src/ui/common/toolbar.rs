#[allow(clippy::wildcard_imports)]
use super::*;

/// Create a pair of prev/next navigation buttons in a linked box.
pub fn build_nav_button_group(prev_tooltip: &str, next_tooltip: &str) -> (Button, Button, GtkBox) {
    let prev_btn = Button::from_icon_name("go-up-symbolic");
    prev_btn.set_tooltip_text(Some(prev_tooltip));
    let next_btn = Button::from_icon_name("go-down-symbolic");
    next_btn.set_tooltip_text(Some(next_tooltip));
    let nav_box = GtkBox::new(Orientation::Horizontal, 0);
    nav_box.add_css_class("linked");
    nav_box.append(&prev_btn);
    nav_box.append(&next_btn);
    (prev_btn, next_btn, nav_box)
}

/// Create undo/redo buttons wired to the active view's buffer.
pub fn build_undo_redo_box(active_view: &Rc<RefCell<TextView>>) -> (Button, Button, GtkBox) {
    let undo_btn = Button::from_icon_name("edit-undo-symbolic");
    undo_btn.set_tooltip_text(Some(&format!("Undo ({}+Z)", primary_key_name())));
    let redo_btn = Button::from_icon_name("edit-redo-symbolic");
    redo_btn.set_tooltip_text(Some(&format!("Redo ({}+Shift+Z)", primary_key_name())));
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
    (undo_btn, redo_btn, undo_redo_box)
}

/// Create blank-line and whitespace filter toggle buttons in a linked box.
pub fn build_filter_toggles(
    ignore_blanks: bool,
    ignore_whitespace: bool,
) -> (ToggleButton, ToggleButton, GtkBox) {
    let blank_toggle = ToggleButton::with_label("Blanks");
    blank_toggle.set_tooltip_text(Some("Ignore blank lines"));
    blank_toggle.set_active(ignore_blanks);
    let ws_toggle = ToggleButton::with_label("Spaces");
    ws_toggle.set_tooltip_text(Some("Ignore whitespace differences"));
    ws_toggle.set_active(ignore_whitespace);
    let filter_box = GtkBox::new(Orientation::Horizontal, 0);
    filter_box.add_css_class("linked");
    filter_box.append(&blank_toggle);
    filter_box.append(&ws_toggle);
    (blank_toggle, ws_toggle, filter_box)
}
