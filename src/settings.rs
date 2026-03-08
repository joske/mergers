use serde::{Deserialize, Serialize};
use std::path::PathBuf;

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(default)]
#[allow(clippy::struct_excessive_bools)]
pub struct Settings {
    pub font: String,
    pub style_scheme: String,
    pub show_line_numbers: bool,
    pub wrap_mode: String,
    pub tab_width: u32,
    pub highlight_current_line: bool,
    pub insert_spaces: bool,
    pub show_whitespace: bool,
    pub hide_hidden_files: bool,
    pub wrap_around_navigation: bool,
    pub ignore_blank_lines: bool,
    pub ignore_whitespace: bool,
    pub dir_filters: Vec<String>,
    pub window_width: i32,
    pub window_height: i32,
    pub window_maximized: bool,
    pub window_fullscreen: bool,
}

impl Default for Settings {
    fn default() -> Self {
        Self {
            font: "Monospace 11".to_string(),
            style_scheme: "adwaita".to_string(),
            show_line_numbers: true,
            wrap_mode: "none".to_string(),
            tab_width: 4,
            highlight_current_line: true,
            insert_spaces: false,
            show_whitespace: false,
            hide_hidden_files: true,
            wrap_around_navigation: false,
            ignore_blank_lines: false,
            ignore_whitespace: false,
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
            window_width: 900,
            window_height: 600,
            window_maximized: false,
            window_fullscreen: false,
        }
    }
}

impl Settings {
    #[must_use]
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

    #[must_use]
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

    #[must_use]
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
        assert!(!s.wrap_around_navigation);
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
        assert_eq!(
            original.wrap_around_navigation,
            parsed.wrap_around_navigation
        );
        assert_eq!(original.insert_spaces, parsed.insert_spaces);
        assert_eq!(original.show_whitespace, parsed.show_whitespace);
        assert_eq!(original.ignore_blank_lines, parsed.ignore_blank_lines);
        assert_eq!(original.ignore_whitespace, parsed.ignore_whitespace);
        assert_eq!(original.dir_filters, parsed.dir_filters);
        assert_eq!(original.window_width, parsed.window_width);
        assert_eq!(original.window_height, parsed.window_height);
        assert_eq!(original.window_maximized, parsed.window_maximized);
        assert_eq!(original.window_fullscreen, parsed.window_fullscreen);
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
    fn test_window_state_defaults() {
        let s = Settings::default();
        assert_eq!(s.window_width, 900);
        assert_eq!(s.window_height, 600);
        assert!(!s.window_maximized);
        assert!(!s.window_fullscreen);
    }

    #[test]
    fn test_window_state_roundtrip() {
        let s = Settings {
            window_width: 1200,
            window_height: 800,
            window_maximized: true,
            window_fullscreen: true,
            ..Settings::default()
        };
        let toml_str = toml::to_string_pretty(&s).unwrap();
        let parsed: Settings = toml::from_str(&toml_str).unwrap();
        assert_eq!(parsed.window_width, 1200);
        assert_eq!(parsed.window_height, 800);
        assert!(parsed.window_maximized);
        assert!(parsed.window_fullscreen);
    }

    #[test]
    fn test_old_config_without_window_fields() {
        let toml_str = r#"
font = "Monospace 11"
tab_width = 8
"#;
        let s: Settings = toml::from_str(toml_str).unwrap();
        assert_eq!(s.tab_width, 8);
        // Window fields get defaults when absent
        assert_eq!(s.window_width, 900);
        assert_eq!(s.window_height, 600);
        assert!(!s.window_maximized);
        assert!(!s.window_fullscreen);
    }

    #[test]
    fn test_config_path_has_mergers() {
        let path = Settings::config_path();
        let path_str = path.to_string_lossy();
        assert!(path_str.contains("mergers"));
        assert!(path_str.ends_with("settings.toml"));
    }
}
