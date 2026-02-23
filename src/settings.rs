use serde::{Deserialize, Serialize};
use std::path::PathBuf;

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(default)]
pub struct Settings {
    pub font: String,
    pub style_scheme: String,
    pub show_line_numbers: bool,
    pub wrap_mode: String,
    pub tab_width: u32,
    pub highlight_current_line: bool,
    pub dir_filters: Vec<String>,
}

impl Default for Settings {
    fn default() -> Self {
        Self {
            font: "Monospace 11".to_string(),
            style_scheme: "Adwaita".to_string(),
            show_line_numbers: true,
            wrap_mode: "none".to_string(),
            tab_width: 4,
            highlight_current_line: true,
            dir_filters: vec![
                ".git".into(),
                ".svn".into(),
                ".hg".into(),
                ".bzr".into(),
                "_darcs".into(),
                ".CVS".into(),
                "__pycache__".into(),
                "node_modules".into(),
                ".DS_Store".into(),
            ],
        }
    }
}

impl Settings {
    pub fn config_path() -> PathBuf {
        let mut p = if let Some(config) = std::env::var_os("XDG_CONFIG_HOME") {
            PathBuf::from(config)
        } else if let Some(home) = std::env::var_os("HOME") {
            PathBuf::from(home).join(".config")
        } else {
            PathBuf::from(".")
        };
        p.push("meld-rs");
        p.push("settings.toml");
        p
    }

    pub fn load() -> Self {
        let path = Self::config_path();
        match std::fs::read_to_string(&path) {
            Ok(contents) => toml::from_str(&contents).unwrap_or_default(),
            Err(_) => Self::default(),
        }
    }

    pub fn save(&self) {
        let path = Self::config_path();
        if let Some(parent) = path.parent() {
            let _ = std::fs::create_dir_all(parent);
        }
        if let Ok(contents) = toml::to_string_pretty(self) {
            let _ = std::fs::write(&path, contents);
        }
    }

    pub fn wrap_mode_gtk(&self) -> gtk4::WrapMode {
        match self.wrap_mode.as_str() {
            "word" => gtk4::WrapMode::Word,
            "char" => gtk4::WrapMode::Char,
            _ => gtk4::WrapMode::None,
        }
    }
}
