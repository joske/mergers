#![allow(
    clippy::cast_sign_loss,
    clippy::cast_possible_truncation,
    clippy::cast_possible_wrap,
    clippy::cast_lossless,
    clippy::cast_precision_loss,
    clippy::needless_pass_by_value,
    clippy::similar_names,
    clippy::too_many_lines,
    clippy::wildcard_imports
)]

use std::{
    cell::{Cell, RefCell},
    collections::{BTreeMap, BTreeSet, HashMap, HashSet},
    fmt::Write as _,
    fs,
    path::{Path, PathBuf},
    rc::Rc,
    sync::mpsc,
    time::{Duration, SystemTime},
};

use chrono::{DateTime, Local};
use gtk4::{
    Adjustment, Application, ApplicationWindow, Box as GtkBox, Button, ColumnView,
    ColumnViewColumn, CssProvider, DrawingArea, Entry, EventControllerFocus, EventControllerKey,
    GestureClick, Image, Label, ListItem, Notebook, Orientation, Paned, PolicyType, PopoverMenu,
    Revealer, ScrolledWindow, SignalListItemFactory, SingleSelection, StringObject, TextBuffer,
    TextSearchFlags, TextTag, TextView, ToggleButton, TreeExpander, TreeListModel, TreeListRow,
    gdk::Display, gio, gio::ListStore, prelude::*,
};
use sourceview5::prelude::*;

use crate::{
    CompareMode,
    myers::{self, DiffChunk, DiffTag},
    settings::Settings,
};

mod common;
mod diff_view;
mod dir_window;
mod file_window;
mod merge_view;
mod preferences;
mod vcs_window;
mod welcome;

use common::*;
use diff_view::*;
use dir_window::*;
use file_window::*;
use merge_view::*;
use preferences::*;
use vcs_window::*;
use welcome::*;

const CSS: &str = r"
.diff-changed { color: #729fcf; font-weight: bold; }
.diff-deleted { color: #f57900; }
.diff-inserted { color: #73d216; }
.diff-missing { color: #888a85; font-style: italic; }
.info-bar { background: #3584e4; padding: 8px 12px; }
.info-bar label { color: white; }
.chunk-label { font-size: 0.9em; }
.linked > button { min-width: 0; padding: 4px 8px; }
.find-bar { background: alpha(@theme_bg_color, 0.95); border-top: 1px solid @borders; padding: 4px 6px; }
.find-bar entry { min-height: 28px; }
.goto-entry { min-height: 28px; }
.dir-pane-focused { border: 2px solid @accent_color; border-radius: 4px; }
";

const SEP: char = '\x1f';

thread_local! {
    static FONT_PROVIDER: CssProvider = CssProvider::new();
    static FONT_REGISTERED: Cell<bool> = const { Cell::new(false) };
    static SAVING_PATHS: RefCell<HashSet<PathBuf>> = RefCell::new(HashSet::new());
}

fn update_font_css(settings: &Settings) {
    let font_desc = gtk4::pango::FontDescription::from_string(&settings.font);
    let css = format!(
        ".meld-editor {{ font-family: \"{}\"; font-size: {}pt; }}",
        font_desc.family().unwrap_or("Monospace".into()),
        font_desc.size() / gtk4::pango::SCALE,
    );
    FONT_PROVIDER.with(|provider| {
        provider.load_from_string(&css);
        FONT_REGISTERED.with(|reg| {
            if !reg.get() {
                gtk4::style_context_add_provider_for_display(
                    &Display::default().expect("GTK display must be available"),
                    provider,
                    gtk4::STYLE_PROVIDER_PRIORITY_APPLICATION + 1,
                );
                reg.set(true);
            }
        });
    });
}

// ─── Main UI ───────────────────────────────────────────────────────────────

pub(crate) fn build_ui(application: &Application, mode: CompareMode) {
    // Ctrl+Q: quit application
    {
        let quit = gio::SimpleAction::new("quit", None);
        let app = application.clone();
        quit.connect_activate(move |_, _| {
            // Close all windows; each window's close-request handler
            // will prompt for unsaved changes if needed.
            for w in app.windows() {
                w.close();
            }
        });
        application.add_action(&quit);
        application.set_accels_for_action("app.quit", &["<Ctrl>q"]);
    }

    application.connect_activate(move |app| {
        let mode = mode.clone();

        // Load CSS
        let provider = CssProvider::new();
        provider.load_from_string(CSS);
        gtk4::style_context_add_provider_for_display(
            &Display::default().expect("GTK display must be available"),
            &provider,
            gtk4::STYLE_PROVIDER_PRIORITY_APPLICATION,
        );

        let settings = Rc::new(RefCell::new(Settings::load()));

        match mode {
            CompareMode::Dirs {
                left,
                right,
                labels: _,
            } => build_dir_window(app, left, right, settings),
            CompareMode::Files {
                left,
                right,
                labels,
            } => build_file_window(app, left, right, &labels, &settings),
            CompareMode::Merge {
                left,
                middle,
                right,
                labels,
            } => build_merge_window(app, left, middle, right, &labels, &settings),
            CompareMode::Vcs { dir } => build_vcs_window(app, dir, settings),
            CompareMode::Welcome => build_welcome_window(app, settings),
        }
    });
}
