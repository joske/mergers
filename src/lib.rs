use std::path::PathBuf;

pub mod myers;
pub mod settings;
pub mod ui;
pub mod vcs;

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
