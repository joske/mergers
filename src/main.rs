use std::path::Path;

use dir_cmp::{full::compare_dirs, Options};
use gtk::builders::ListViewBuilder;
use walkdir::WalkDir;

use gtk::prelude::*;
use gtk::{glib, Application, ApplicationWindow, };
use gtk4::{self as gtk, ListView};

// fn main() {
//     let options = Options {
//         ignore_equal: false,
//         ignore_left_only: false,
//         ignore_right_only: false,
//         filter: None,
//         recursive: true,
//     };
//     let left = Path::new("/home/jos/Pictures");
//     let right = Path::new("/home/jos/Pictures");
//     let result = compare_dirs(left, right, options).unwrap();
//     for r in result {
//         println!("{r:?}");
//     }
// }
fn main() -> glib::ExitCode {
    let application = Application::builder()
        .application_id("com.example.FirstGtkApp")
        .build();

    application.connect_activate(|app| {
        let window = ApplicationWindow::builder()
            .application(app)
            .title("First GTK Program")
            .default_width(350)
            .default_height(70)
            .build();

        let list_view = ListView::new(None, None);
        let root = WalkDir::new("/home/jos/Pictures");
        for entry in root.into_iter().filter_map(|e| e.ok()) {
            println!("{}", entry.path().display());
        }
        window.set_child(Some(&list_view));

        window.present();
    });

    application.run()
}
