use std::{cmp::Ordering, fs::read_dir, io, path::Path};

#[derive(Debug, Clone)]
pub(crate) struct Entry {
    name: String,
    is_dir: bool,
}

impl Entry {
    pub(crate) fn name(&self) -> &str {
        self.name.as_str()
    }
}

impl Ord for Entry {
    fn cmp(&self, other: &Self) -> Ordering {
        self.name.cmp(&other.name)
    }
}

impl PartialOrd for Entry {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for Entry {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Eq for Entry {}

#[derive(Debug)]
pub(crate) struct Folder {
    name: String,
    children: Vec<Entry>,
}
impl Folder {
    pub(crate) fn children(&self) -> Vec<Entry> {
        self.children.clone()
    }
}

impl PartialEq for Folder {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.children == other.children
    }
}

impl Eq for Folder {}

pub(crate) fn diff(left: &Path, right: &Path) -> io::Result<(Folder, Folder)> {
    let mut left_dir = read_dir(left)?
        .map(|res| {
            res.map(|e| Entry {
                name: e.file_name().into_string().unwrap(),
                is_dir: e.path().is_dir(),
            })
        })
        .collect::<Result<Vec<Entry>, io::Error>>()?;
    left_dir.sort();
    let left_folder = Folder {
        name: (*left.file_name().unwrap()).to_str().unwrap().to_owned(),
        children: left_dir,
    };

    let mut right_dir = read_dir(right)?
        .map(|res| {
            res.map(|e| Entry {
                name: e.file_name().into_string().unwrap(),
                is_dir: e.path().is_dir(),
            })
        })
        .collect::<Result<Vec<Entry>, io::Error>>()?;
    right_dir.sort();
    let right_folder = Folder {
        name: (*right.file_name().unwrap()).to_str().unwrap().to_owned(),
        children: right_dir,
    };
    Ok((left_folder, right_folder))
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use super::diff;

    #[test]
    fn test() {
        let diff = diff(Path::new("/tmp"), Path::new("/tmp")).unwrap();
        assert_eq!(diff.0, diff.1);
    }
}
