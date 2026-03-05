#[allow(clippy::wildcard_imports)]
use super::*;

pub fn clear_search_tags(buf: &TextBuffer) {
    let start = buf.start_iter();
    let end = buf.end_iter();
    for name in &["search-match", "search-current"] {
        if let Some(tag) = buf.tag_table().lookup(name) {
            buf.remove_tag(&tag, &start, &end);
        }
    }
}

/// Highlight all occurrences of `needle` in `buf`. Returns match count.
pub fn highlight_search_matches(buf: &TextBuffer, needle: &str) -> usize {
    clear_search_tags(buf);
    if needle.is_empty() {
        return 0;
    }
    let mut count = 0;
    let mut iter = buf.start_iter();
    while let Some((match_start, match_end)) =
        iter.forward_search(needle, TextSearchFlags::CASE_INSENSITIVE, None)
    {
        buf.apply_tag_by_name("search-match", &match_start, &match_end);
        count += 1;
        iter = match_end;
    }
    count
}

/// Find the next (or previous) match starting from `from`. Returns (start, end) iters.
pub fn find_next_match(
    buf: &TextBuffer,
    needle: &str,
    from: &gtk4::TextIter,
    forward: bool,
) -> Option<(gtk4::TextIter, gtk4::TextIter)> {
    if needle.is_empty() {
        return None;
    }
    if forward {
        // Start one character ahead so we don't re-find the current match
        let mut start = *from;
        start.forward_char();
        let result = start.forward_search(needle, TextSearchFlags::CASE_INSENSITIVE, None);
        if result.is_some() {
            return result;
        }
        // Wrap around
        buf.start_iter()
            .forward_search(needle, TextSearchFlags::CASE_INSENSITIVE, None)
    } else {
        let result = from.backward_search(needle, TextSearchFlags::CASE_INSENSITIVE, None);
        if result.is_some() {
            return result;
        }
        // Wrap around
        buf.end_iter()
            .backward_search(needle, TextSearchFlags::CASE_INSENSITIVE, None)
    }
}
