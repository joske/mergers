use std::{cmp::Ordering, fs::read_dir, io, path::Path};

#[derive(Debug, Clone)]
pub(crate) struct Entry {
    name: String,
    len: u64,
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
    let left_folder = read_folder(left)?;
    let right_folder = read_folder(right)?;

    Ok((left_folder, right_folder))
}

fn read_folder(path: &Path) -> Result<Folder, io::Error> {
    let mut dir = read_dir(path)?
        .map(|res| {
            res.map(|e| {
                let is_dir = e.path().is_dir();
                Entry {
                    name: e.file_name().into_string().unwrap(),
                    len: e.metadata().unwrap().len(),
                    is_dir,
                }
            })
        })
        .collect::<Result<Vec<Entry>, io::Error>>()?;
    dir.sort();
    let folder = Folder {
        name: (*path.file_name().unwrap()).to_str().unwrap().to_owned(),
        children: dir,
    };
    Ok(folder)
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use dircmp::Comparison;
    use walkdir::WalkDir;

    use super::diff;

    #[test]
    fn test() {
        let diff = diff(Path::new("/tmp"), Path::new("/tmp")).unwrap();
        assert_eq!(diff.0, diff.1);
    }

    #[test]
    fn test_diff() {
        let diff = Comparison::default()
            .compare(
                Path::new("/home/jos/tmp/cmp1/"),
                Path::new("/home/jos/tmp/cmp2"),
            )
            .unwrap();
        println!("{:?}", diff);
    }

    #[test]
    fn test_diff2() {
        let dir = WalkDir::new("/home/jos/tmp/cmp1").sort_by_file_name();
        println!("{:?}", dir.into_iter());
    }
}
