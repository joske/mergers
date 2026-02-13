use gio::prelude::ApplicationExtManual;
use gtk4::{glib, Application};

mod myers;
mod ui;

fn main() -> glib::ExitCode {
    let args: Vec<String> = std::env::args().collect();

    let (left_dir, right_dir) = if args.len() >= 3 {
        (args[1].clone(), args[2].clone())
    } else {
        (
            "/home/jos/tmp/cmp1".to_string(),
            "/home/jos/tmp/cmp2".to_string(),
        )
    };

    let application = Application::builder()
        .application_id("com.example.FirstGtkApp")
        .build();

    ui::build_ui(&application, left_dir, right_dir);

    // Pass empty args so GTK doesn't try to parse our directory args
    application.run_with_args::<String>(&[])
}
