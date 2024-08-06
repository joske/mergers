use gtk4::gio::ListStore;
use gtk4::{glib, Application, ApplicationWindow, Label, ListItem};
use gtk4::{prelude::*, PolicyType, ScrolledWindow};
use gtk4::{ListView, MultiSelection, SignalListItemFactory, StringObject};
use walkdir::WalkDir;

fn main() -> glib::ExitCode {
    let application = Application::builder()
        .application_id("com.example.FirstGtkApp")
        .build();

    build_ui(&application);

    application.run()
}

fn build_ui(application: &Application) {
    application.connect_activate(|app| {
        let left_model = ListStore::new::<StringObject>();

        // store data in the model
        let root = WalkDir::new("/home/jos/Pictures").min_depth(1);
        for entry in root.into_iter().filter_map(|e| e.ok()) {
            let object = StringObject::new(&entry.path().display().to_string());
            left_model.append(&object);
        }

        // create the tree view
        let left_selection_model = MultiSelection::new(Some(left_model));
        let left_factory = SignalListItemFactory::new();
        left_factory.connect_setup(move |_, list_item| {
            let label = Label::new(None);
            list_item
                .downcast_ref::<ListItem>()
                .expect("Needs to be ListItem")
                .set_child(Some(&label));
        });
        left_factory.connect_bind(move |_, list_item| {
            // Get `IntegerObject` from `ListItem`
            let integer_object = list_item
                .downcast_ref::<ListItem>()
                .expect("Needs to be ListItem")
                .item()
                .and_downcast::<StringObject>()
                .expect("The item has to be an `IntegerObject`.");

            // Get `Label` from `ListItem`
            let label = list_item
                .downcast_ref::<ListItem>()
                .expect("Needs to be ListItem")
                .child()
                .and_downcast::<Label>()
                .expect("The child has to be a `Label`.");

            // Set "label" to "number"
            label.set_label(&integer_object.string());
        });

        let left_tree = ListView::new(Some(left_selection_model), Some(left_factory));
        let scrolled_window = ScrolledWindow::builder()
            .hscrollbar_policy(PolicyType::Never) // Disable horizontal scrolling
            .min_content_width(360)
            .child(&left_tree)
            .build();

        let window = ApplicationWindow::builder()
            .application(app)
            .title("Diff")
            .default_width(350)
            .default_height(70)
            .child(&scrolled_window)
            .build();

        window.present();
    });
}

// alternative code for comparing directories
// use dir_cmp::{full::compare_dirs, Options};
// use std::path::Path;
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
