use std::path::PathBuf;

pub mod myers;
pub mod patch;
pub mod settings;
pub mod ui;
pub mod vcs;

/// Re-exports for benchmarks. Not part of the public API.
#[doc(hidden)]
pub mod _bench {
    pub use crate::ui::{
        common::{
            editor::conflict_flags,
            gutter::{merged_gutter_chunks, middle_conflict_regions},
        },
        diff_state::{Side, compute_chunk_map_rects, find_next_chunk, format_chunk_label},
        merge_state::{
            conflict_at_cursor, conflict_at_cursor_fast, find_conflict_blocks,
            find_conflict_markers_in_text, merge_change_indices,
        },
    };
}

#[derive(Clone, Debug)]
pub enum CompareMode {
    Files {
        left: PathBuf,
        right: PathBuf,
        labels: Vec<String>,
    },
    Dirs {
        left: PathBuf,
        right: PathBuf,
        labels: Vec<String>,
    },
    Merge {
        left: PathBuf,
        middle: PathBuf,
        right: PathBuf,
        labels: Vec<String>,
    },
    Vcs {
        dir: PathBuf,
    },
    Welcome,
}
