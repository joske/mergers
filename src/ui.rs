use gtk4::gio::ListStore;
use gtk4::{prelude::*, Orientation, PolicyType, ScrolledWindow};
use gtk4::{Application, ApplicationWindow, Label, ListItem};
use gtk4::{ListView, MultiSelection, SignalListItemFactory, StringObject};
use walkdir::WalkDir;

// use std::path::Path;
// use crate::diff;

pub(crate) fn build_ui(application: &Application) {
    application.connect_activate(|app| {
        let left_model = ListStore::new::<StringObject>();
        let right_model = ListStore::new::<StringObject>();

        // store data in the model
        let left = WalkDir::new("/home/jos/tmp/cmp1")
            .min_depth(1)
            .sort_by_file_name();
        let right = WalkDir::new("/home/jos/tmp/cmp2")
            .min_depth(1)
            .sort_by_file_name();
        // let (left, right) = diff::diff(
        //     Path::new("/home/jos/tmp/cmp1"),
        //     Path::new("/home/jos/tmp/cmp2"),
        // )
        // .unwrap();
        for entry in left.into_iter().filter_map(|e| e.ok()) {
            let object = StringObject::new(entry.path().to_str().unwrap());
            left_model.append(&object);
        }
        for entry in right.into_iter().filter_map(|e| e.ok()) {
            let object = StringObject::new(entry.path().to_str().unwrap());
            right_model.append(&object);
        }

        let left_tree = create_tree(left_model);
        let right_tree = create_tree(right_model);

        let left_scrolled_window = ScrolledWindow::builder()
            .hscrollbar_policy(PolicyType::Never) // Disable horizontal scrolling
            .min_content_width(360)
            .child(&left_tree)
            .build();

        let right_scrolled_window = ScrolledWindow::builder()
            .hscrollbar_policy(PolicyType::Never) // Disable horizontal scrolling
            .min_content_width(360)
            .child(&right_tree)
            .build();

        let main = gtk4::Paned::new(Orientation::Horizontal);
        main.set_start_child(Some(&left_scrolled_window));
        main.set_end_child(Some(&right_scrolled_window));

        let window = ApplicationWindow::builder()
            .application(app)
            .title("Diff")
            .default_width(350)
            .default_height(350)
            .child(&main)
            .build();

        window.present();
    });
}

fn create_tree(store: ListStore) -> ListView {
    // create the tree view
    let selection_model = MultiSelection::new(Some(store));
    let factory = SignalListItemFactory::new();
    factory.connect_setup(move |_, list_item| {
        let label = Label::new(None);
        list_item
            .downcast_ref::<ListItem>()
            .expect("Needs to be ListItem")
            .set_child(Some(&label));
    });
    factory.connect_bind(move |_, list_item| {
        // Get `StringObject` from `ListItem`
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

    ListView::new(Some(selection_model), Some(factory))
}
