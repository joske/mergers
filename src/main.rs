use gio::prelude::ApplicationExtManual;
use gtk4::{glib, Application};

mod myers;
mod ui;

fn main() -> glib::ExitCode {
    let application = Application::builder()
        .application_id("com.example.FirstGtkApp")
        .build();

    ui::build_ui(&application);

    application.run()
}
