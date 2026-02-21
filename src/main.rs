use std::path::PathBuf;

use clap::Parser;
use gio::prelude::ApplicationExtManual;
use gtk4::{Application, glib};

mod myers;
mod ui;

#[derive(Parser)]
#[command(name = "meld-rs", about = "Visual diff and merge tool")]
struct Cli {
    /// Paths to compare (2 files or 2 directories, or 3 files for merge)
    paths: Vec<PathBuf>,

    /// Output file for 3-way merge result
    #[arg(short, long)]
    output: Option<PathBuf>,
}

#[derive(Clone)]
pub enum CompareMode {
    Files {
        left: PathBuf,
        right: PathBuf,
    },
    Dirs {
        left: PathBuf,
        right: PathBuf,
    },
    Merge {
        left: PathBuf,
        middle: PathBuf,
        right: PathBuf,
        output: Option<PathBuf>,
    },
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
            output: cli.output,
        }
    } else if cli.paths.len() == 2 {
        let left = &cli.paths[0];
        let right = &cli.paths[1];

        if left.is_file() && right.is_file() {
            CompareMode::Files {
                left: left.clone(),
                right: right.clone(),
            }
        } else if left.is_dir() && right.is_dir() {
            CompareMode::Dirs {
                left: left.clone(),
                right: right.clone(),
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
    } else {
        eprintln!("Usage: meld-rs <LEFT> <RIGHT>");
        eprintln!("       meld-rs <LOCAL> <MERGED> <REMOTE>");
        eprintln!("       meld-rs <LOCAL> <BASE> <REMOTE> --output MERGED");
        std::process::exit(1);
    };

    let application = Application::builder()
        .application_id("com.example.MeldRs")
        .build();

    ui::build_ui(&application, mode);

    // Pass empty args so GTK doesn't try to parse our directory args
    application.run_with_args::<String>(&[])
}
