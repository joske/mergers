#![windows_subsystem = "windows"]

use std::path::PathBuf;

use clap::Parser;
use gio::prelude::ApplicationExtManual;
use gtk4::{Application, glib};

mod myers;
mod settings;
mod ui;
mod vcs;

#[derive(Parser)]
#[command(name = "mergers", version, about = "Visual diff and merge tool")]
struct Cli {
    /// Paths to compare (2 files or 2 directories, or 3 files for 3-way merge)
    paths: Vec<PathBuf>,

    /// Custom labels for panes (one per pane)
    #[arg(short = 'L', long = "label")]
    labels: Vec<String>,
}

#[derive(Clone)]
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

fn main() -> glib::ExitCode {
    #[cfg(target_os = "macos")]
    {
        // Workaround for flickering issues on macOS with GTK 4.14+ (the new 'ngl' renderer)
        // especially on external 4K monitors. Forcing the legacy 'gl' renderer or 'cairo'
        // usually resolves it.
        if std::env::var_os("GSK_RENDERER").is_none() {
            unsafe {
                std::env::set_var("GSK_RENDERER", "gl");
            }
        }
    }

    let cli = Cli::parse();

    let mode = if cli.paths.len() == 3 {
        let left = &cli.paths[0];
        let middle = &cli.paths[1];
        let right = &cli.paths[2];

        for p in [left, middle, right] {
            if !p.is_file() {
                eprintln!("Error: '{}' is not a file", p.display());
                std::process::exit(1);
            }
        }

        CompareMode::Merge {
            left: left.clone(),
            middle: middle.clone(),
            right: right.clone(),
            labels: cli.labels.clone(),
        }
    } else if cli.paths.len() == 2 {
        let left = &cli.paths[0];
        let right = &cli.paths[1];

        if left.is_file() && right.is_file() {
            CompareMode::Files {
                left: left.clone(),
                right: right.clone(),
                labels: cli.labels.clone(),
            }
        } else if left.is_dir() && right.is_dir() {
            CompareMode::Dirs {
                left: left.clone(),
                right: right.clone(),
                labels: cli.labels.clone(),
            }
        } else if !left.exists() {
            eprintln!("Error: '{}' does not exist", left.display());
            std::process::exit(1);
        } else if !right.exists() {
            eprintln!("Error: '{}' does not exist", right.display());
            std::process::exit(1);
        } else {
            eprintln!("Error: cannot compare a file with a directory");
            std::process::exit(1);
        }
    } else if cli.paths.len() == 1 {
        let path = &cli.paths[0];
        if path.is_dir() && vcs::is_git_repo(path) {
            CompareMode::Vcs { dir: path.clone() }
        } else if path.is_dir() {
            eprintln!("Error: '{}' is not inside a git repository", path.display());
            eprintln!("For directory comparison, provide two directories.");
            std::process::exit(1);
        } else {
            eprintln!("Error: single file argument not supported. Provide 2 files to compare.");
            std::process::exit(1);
        }
    } else if cli.paths.is_empty() {
        CompareMode::Welcome
    } else {
        eprintln!(
            "Error: expected 0-3 paths, got {}. Usage: mergers [LEFT RIGHT] or [LEFT MIDDLE RIGHT]",
            cli.paths.len()
        );
        std::process::exit(1);
    };

    let application = Application::builder().application_id("mergers").build();

    ui::build_ui(&application, mode);

    // Pass empty args so GTK doesn't try to parse our directory args
    application.run_with_args::<String>(&[])
}
