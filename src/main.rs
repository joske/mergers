#![windows_subsystem = "windows"]

use std::{
    io::{BufRead, BufReader},
    path::{Path, PathBuf},
};

use clap::Parser;
use gio::prelude::{ApplicationExt, ApplicationExtManual};
use gtk4::{Application, glib};

use mergers::{CompareMode, patch, ui, vcs};

#[derive(Parser)]
#[command(name = "mergers", version, about = "Visual diff and merge tool")]
struct Cli {
    /// Paths to compare (2 files, 2 dirs, 3 files for merge, or file/dir + patch)
    paths: Vec<PathBuf>,

    /// Custom labels for panes (one per pane)
    #[arg(short = 'L', long = "label", value_name = "LABEL")]
    labels: Vec<String>,
}

fn is_right_a_patch(path: &Path) -> bool {
    if let Some(ext) = path.extension().and_then(|e| e.to_str())
        && matches!(ext, "patch" | "diff")
    {
        return true;
    }
    // Read only the first 50 lines instead of the entire file
    if let Ok(file) = std::fs::File::open(path) {
        let reader = BufReader::new(file);
        let lines: Vec<String> = reader.lines().take(50).filter_map(Result::ok).collect();
        let preview = lines.join("\n");
        return patch::is_patch_file(&preview);
    }
    false
}

fn main() -> glib::ExitCode {
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
            if is_right_a_patch(right) {
                CompareMode::Patch {
                    base: left.clone(),
                    patch: right.clone(),
                    labels: cli.labels.clone(),
                }
            } else {
                CompareMode::Files {
                    left: left.clone(),
                    right: right.clone(),
                    labels: cli.labels.clone(),
                }
            }
        } else if left.is_dir() && right.is_file() {
            if is_right_a_patch(right) {
                CompareMode::Patch {
                    base: left.clone(),
                    patch: right.clone(),
                    labels: cli.labels.clone(),
                }
            } else {
                eprintln!("Error: cannot compare a directory with a file");
                std::process::exit(1);
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
        if !path.exists() {
            eprintln!("Error: '{}' does not exist", path.display());
            std::process::exit(1);
        } else if path.is_dir() && vcs::is_git_repo(path) {
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

    // Clean up patch temp dirs on normal shutdown
    application.connect_shutdown(|_| {
        ui::cleanup_patch_temp_dirs();
    });

    ui::build_ui(&application, mode);

    // Set up signal handler *after* GTK init so it doesn't get overridden.
    // ctrlc handles SIGINT and SIGTERM.
    ctrlc::set_handler(|| {
        ui::cleanup_patch_temp_dirs();
        std::process::exit(1);
    })
    .ok();

    // Pass empty args so GTK doesn't try to parse our directory args
    let code = application.run_with_args::<String>(&[]);

    // Final cleanup in case shutdown handler didn't fire
    ui::cleanup_patch_temp_dirs();

    code
}
