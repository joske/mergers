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
        } else if let Some(appdata) = std::env::var_os("APPDATA") {
            PathBuf::from(appdata)
        } else if let Some(home) = std::env::var_os("HOME") {
            PathBuf::from(home).join(".config")
        } else {
            PathBuf::from(".")
        };
        p.push("mergers");
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_defaults() {
        let s = Settings::default();
        assert_eq!(s.font, "Monospace 11");
        assert!(s.show_line_numbers);
        assert_eq!(s.tab_width, 4);
        assert!(s.dir_filters.contains(&".git".to_string()));
    }

    #[test]
    fn test_serde_roundtrip() {
        let original = Settings::default();
        let toml_str = toml::to_string_pretty(&original).unwrap();
        let parsed: Settings = toml::from_str(&toml_str).unwrap();
        assert_eq!(original.font, parsed.font);
        assert_eq!(original.style_scheme, parsed.style_scheme);
        assert_eq!(original.tab_width, parsed.tab_width);
        assert_eq!(original.show_line_numbers, parsed.show_line_numbers);
        assert_eq!(original.wrap_mode, parsed.wrap_mode);
        assert_eq!(original.dir_filters, parsed.dir_filters);
    }

    #[test]
    fn test_deserialize_missing_fields() {
        let toml_str = r#"font = "Hack 12""#;
        let s: Settings = toml::from_str(toml_str).unwrap();
        assert_eq!(s.font, "Hack 12");
        // Other fields should get defaults
        assert_eq!(s.tab_width, 4);
        assert!(s.show_line_numbers);
    }

    #[test]
    fn test_config_path_has_mergers() {
        let path = Settings::config_path();
        let path_str = path.to_string_lossy();
        assert!(path_str.contains("mergers"));
        assert!(path_str.ends_with("settings.toml"));
    }
}
