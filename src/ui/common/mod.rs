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

pub(super) struct FileTab {
    pub(super) id: u64,
    pub(super) rel_path: String,
    pub(super) widget: GtkBox,
    pub(super) left_path: Rc<RefCell<String>>,
    pub(super) right_path: Rc<RefCell<String>>,
    pub(super) left_buf: TextBuffer,
    pub(super) right_buf: TextBuffer,
    pub(super) left_save: Button,
    pub(super) right_save: Button,
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
    // Don't overwrite unsaved user edits
    if tab.left_save.is_sensitive() || tab.right_save.is_sensitive() {
        return true; // not a read failure — just skip
    }

    // Skip tabs that aren't backed by two on-disk files (e.g. merge tabs,
    // blank comparisons).
    if tab.left_path.borrow().is_empty() || tab.right_path.borrow().is_empty() {
        return true;
    }

    // Use tab.left_path / right_path (which track swaps) instead of
    // reconstructing from left_dir + rel_path, so reloads respect swapped panes.
    let left_content = read_file_for_reload(Path::new(&*tab.left_path.borrow()));
    let right_content = read_file_for_reload(Path::new(&*tab.right_path.borrow()));

    // Skip panes where read failed or file became binary
    let Some(left_content) = left_content else {
        return false;
    };
    let Some(right_content) = right_content else {
        return false;
    };

    // Only reset buffers if the on-disk content actually differs from what's in the buffer
    let cur_left = tab
        .left_buf
        .text(&tab.left_buf.start_iter(), &tab.left_buf.end_iter(), false);
    let cur_right = tab.right_buf.text(
        &tab.right_buf.start_iter(),
        &tab.right_buf.end_iter(),
        false,
    );

    if cur_left.as_str() == left_content && cur_right.as_str() == right_content {
        return true; // nothing changed on disk vs buffer
    }

    // set_text triggers connect_changed which schedules refresh_diff with
    // the current filter state (ignore_blanks / ignore_whitespace).
    // We must NOT run a separate diff here — it would race and potentially
    // overwrite the filtered result with an unfiltered one.
    tab.left_buf.set_text(&left_content);
    tab.right_buf.set_text(&right_content);
    // Buffer now matches disk — clear unsaved-changes indicator.
    // (set_text triggers the save-button connect_changed which sets
    // sensitive=true, so we reset it after.)
    tab.left_save.set_sensitive(false);
    tab.right_save.set_sensitive(false);
    true
}
