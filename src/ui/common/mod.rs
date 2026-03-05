#[allow(clippy::wildcard_imports)]
use super::*;

use super::diff_state;
pub(super) use diff_state::Side;

mod chunk_map;
mod editor;
mod file_watcher;
mod find_bar;
mod gutter;
mod helpers;
mod navigation;
mod scroll_sync;
mod search;
mod tabs;
mod toolbar;

pub(super) use chunk_map::*;
pub(super) use editor::*;
pub(super) use file_watcher::*;
pub(super) use find_bar::*;
pub(super) use gutter::*;
pub(super) use helpers::*;
pub(super) use navigation::*;
pub(super) use scroll_sync::*;
pub(super) use search::*;
pub(super) use tabs::*;
pub(super) use toolbar::*;

#[cfg(test)]
mod tests;

// ─── Data types ────────────────────────────────────────────────────────────

#[derive(Debug, Clone, Copy, PartialEq)]
pub(super) enum FileStatus {
    Same,
    Different,
    LeftOnly,
    RightOnly,
}

impl FileStatus {
    pub(super) fn code(self) -> &'static str {
        match self {
            Self::Same => "S",
            Self::Different => "D",
            Self::LeftOnly => "L",
            Self::RightOnly => "R",
        }
    }
}

pub(super) struct DirMeta {
    pub(super) size: Option<u64>,
    pub(super) mtime: Option<SystemTime>,
    pub(super) is_dir: bool,
}

pub(super) struct PaneInfo {
    pub(super) path: Rc<RefCell<String>>,
    pub(super) buf: TextBuffer,
    pub(super) save: Button,
}

pub(super) enum FileTab {
    Diff {
        id: u64,
        rel_path: String,
        widget: GtkBox,
        left: PaneInfo,
        right: PaneInfo,
    },
    Merge {
        id: u64,
        rel_path: String,
        widget: GtkBox,
        middle: PaneInfo,
    },
}

impl FileTab {
    pub(super) fn id(&self) -> u64 {
        match self {
            Self::Diff { id, .. } | Self::Merge { id, .. } => *id,
        }
    }

    pub(super) fn rel_path(&self) -> &str {
        match self {
            Self::Diff { rel_path, .. } | Self::Merge { rel_path, .. } => rel_path,
        }
    }

    pub(super) fn widget(&self) -> &GtkBox {
        match self {
            Self::Diff { widget, .. } | Self::Merge { widget, .. } => widget,
        }
    }

    /// Returns panes that can be saved / checked for unsaved changes.
    pub(super) fn saveable_panes(&self) -> Vec<&PaneInfo> {
        match self {
            Self::Diff { left, right, .. } => vec![left, right],
            Self::Merge { middle, .. } => vec![middle],
        }
    }

    /// Whether this tab should be reloaded from disk on file-change events.
    /// Only 2-way diff tabs backed by two real on-disk files are reloadable.
    pub(super) fn is_reloadable(&self) -> bool {
        match self {
            Self::Diff { left, right, .. } => {
                !left.path.borrow().is_empty() && !right.path.borrow().is_empty()
            }
            Self::Merge { .. } => false,
        }
    }
}

pub(super) static NEXT_TAB_ID: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);

pub(super) fn mark_saving(path: &Path) {
    let p = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());
    SAVING_PATHS.with(|s| s.borrow_mut().insert(p.clone()));
    gtk4::glib::timeout_add_local_once(Duration::from_millis(600), move || {
        SAVING_PATHS.with(|s| s.borrow_mut().remove(&p));
    });
}

pub(super) fn is_saving(paths: &[&Path]) -> bool {
    SAVING_PATHS.with(|s| {
        let set = s.borrow();
        paths.iter().any(|p| {
            let canon = p.canonicalize().unwrap_or_else(|_| p.to_path_buf());
            set.contains(&canon)
        })
    })
}

pub(super) fn is_saving_under(roots: &[&Path]) -> bool {
    SAVING_PATHS.with(|s| {
        let set = s.borrow();
        let canon_roots: Vec<PathBuf> = roots
            .iter()
            .map(|r| r.canonicalize().unwrap_or_else(|_| r.to_path_buf()))
            .collect();
        set.iter()
            .any(|saving_path| canon_roots.iter().any(|root| saving_path.starts_with(root)))
    })
}

pub(super) fn apply_diff_tags(left_buf: &TextBuffer, right_buf: &TextBuffer, chunks: &[DiffChunk]) {
    for chunk in chunks {
        if chunk.tag == DiffTag::Replace {
            apply_inline_tags(left_buf, right_buf, chunk);
        }
    }
}

/// 3-way merge tagging: apply inline word-level tags only.
/// Chunk backgrounds and conflict overlays are drawn via Cairo.
pub(super) fn apply_merge_tags(
    left_buf: &TextBuffer,
    middle_buf: &TextBuffer,
    right_buf: &TextBuffer,
    left_chunks: &[DiffChunk],
    right_chunks: &[DiffChunk],
) {
    // left_chunks = diff(left, middle): a-side = left, b-side = middle
    for chunk in left_chunks {
        if chunk.tag == DiffTag::Replace {
            apply_inline_tags(left_buf, middle_buf, chunk);
        }
    }
    // right_chunks = diff(middle, right): a-side = middle, b-side = right
    for chunk in right_chunks {
        if chunk.tag == DiffTag::Replace {
            apply_inline_tags(middle_buf, right_buf, chunk);
        }
    }
}

/// Reload a file tab from disk. Returns `false` if a read failed (binary or
/// I/O error) so the caller can keep dirty and retry on the next tick.
pub(super) fn reload_file_tab(tab: &FileTab) -> bool {
    if !tab.is_reloadable() {
        return true;
    }

    // Don't overwrite unsaved user edits
    if tab.saveable_panes().iter().any(|p| p.save.is_sensitive()) {
        return true;
    }

    let FileTab::Diff { left, right, .. } = tab else {
        return true;
    };

    let left_content = read_file_for_reload(Path::new(&*left.path.borrow()));
    let right_content = read_file_for_reload(Path::new(&*right.path.borrow()));

    let Some(left_content) = left_content else {
        return false;
    };
    let Some(right_content) = right_content else {
        return false;
    };

    let cur_left = left
        .buf
        .text(&left.buf.start_iter(), &left.buf.end_iter(), false);
    let cur_right = right
        .buf
        .text(&right.buf.start_iter(), &right.buf.end_iter(), false);

    if cur_left.as_str() == left_content && cur_right.as_str() == right_content {
        return true;
    }

    left.buf.set_text(&left_content);
    right.buf.set_text(&right_content);
    left.save.set_sensitive(false);
    right.save.set_sensitive(false);
    true
}
