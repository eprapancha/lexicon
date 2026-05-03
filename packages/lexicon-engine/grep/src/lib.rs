use std::io;

use grep_matcher::Matcher;
use grep_regex::RegexMatcherBuilder;
use grep_searcher::sinks::UTF8;
use grep_searcher::SearcherBuilder;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct GrepMatch {
    pub line_number: u64,
    pub line_text: String,
    pub match_start: usize,
    pub match_end: usize,
}

pub struct GrepOptions {
    pub case_insensitive: bool,
    pub max_count: Option<u64>,
    pub context_lines: usize,
    pub fixed_strings: bool,
}

impl Default for GrepOptions {
    fn default() -> Self {
        GrepOptions {
            case_insensitive: false,
            max_count: None,
            context_lines: 0,
            fixed_strings: false,
        }
    }
}

/// Search a byte slice for pattern matches. Returns structured results.
pub fn search_slice(
    pattern: &str,
    content: &[u8],
    options: &GrepOptions,
) -> Result<Vec<GrepMatch>, String> {
    let matcher = build_matcher(pattern, options)?;
    let mut searcher = build_searcher(options);
    let mut matches = Vec::new();

    searcher
        .search_slice(
            &matcher,
            content,
            UTF8(|line_num, line| {
                if let Some(m) = matcher
                    .find(line.as_bytes())
                    .map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?
                {
                    matches.push(GrepMatch {
                        line_number: line_num,
                        line_text: line.trim_end_matches('\n').trim_end_matches('\r').to_string(),
                        match_start: m.start(),
                        match_end: m.end(),
                    });
                }
                let keep_going = options
                    .max_count
                    .map_or(true, |max| (matches.len() as u64) < max);
                Ok(keep_going)
            }),
        )
        .map_err(|e| e.to_string())?;

    Ok(matches)
}

fn build_matcher(
    pattern: &str,
    options: &GrepOptions,
) -> Result<grep_regex::RegexMatcher, String> {
    let mut builder = RegexMatcherBuilder::new();
    builder.case_insensitive(options.case_insensitive);
    if options.fixed_strings {
        builder.fixed_strings(true);
    }
    builder.build(pattern).map_err(|e| e.to_string())
}

fn build_searcher(options: &GrepOptions) -> grep_searcher::Searcher {
    let mut builder = SearcherBuilder::new();
    builder.line_number(true);
    if options.context_lines > 0 {
        builder.after_context(options.context_lines);
        builder.before_context(options.context_lines);
    }
    builder.build()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_search() {
        let content = b"hello world\nfoo bar\nhello again\n";
        let matches = search_slice("hello", content, &GrepOptions::default()).unwrap();
        assert_eq!(matches.len(), 2);
        assert_eq!(matches[0].line_number, 1);
        assert_eq!(matches[0].line_text, "hello world");
        assert_eq!(matches[1].line_number, 3);
        assert_eq!(matches[1].line_text, "hello again");
    }

    #[test]
    fn test_match_positions() {
        let content = b"foo hello bar\n";
        let matches = search_slice("hello", content, &GrepOptions::default()).unwrap();
        assert_eq!(matches.len(), 1);
        assert_eq!(matches[0].match_start, 4);
        assert_eq!(matches[0].match_end, 9);
    }

    #[test]
    fn test_case_insensitive() {
        let content = b"Hello World\nhello world\n";
        let opts = GrepOptions {
            case_insensitive: true,
            ..Default::default()
        };
        let matches = search_slice("hello", content, &opts).unwrap();
        assert_eq!(matches.len(), 2);
    }

    #[test]
    fn test_case_sensitive_default() {
        let content = b"Hello World\nhello world\n";
        let matches = search_slice("hello", content, &GrepOptions::default()).unwrap();
        assert_eq!(matches.len(), 1);
        assert_eq!(matches[0].line_number, 2);
    }

    #[test]
    fn test_regex_search() {
        let content = b"foo123\nbar456\nfoo789\n";
        let matches = search_slice("foo\\d+", content, &GrepOptions::default()).unwrap();
        assert_eq!(matches.len(), 2);
        assert_eq!(matches[0].line_text, "foo123");
        assert_eq!(matches[1].line_text, "foo789");
    }

    #[test]
    fn test_max_count() {
        let content = b"a\na\na\na\na\n";
        let opts = GrepOptions {
            max_count: Some(3),
            ..Default::default()
        };
        let matches = search_slice("a", content, &opts).unwrap();
        assert_eq!(matches.len(), 3);
    }

    #[test]
    fn test_fixed_strings() {
        let content = b"foo.bar\nfooXbar\n";
        let opts = GrepOptions {
            fixed_strings: true,
            ..Default::default()
        };
        let matches = search_slice("foo.bar", content, &opts).unwrap();
        assert_eq!(matches.len(), 1);
        assert_eq!(matches[0].line_text, "foo.bar");
    }

    #[test]
    fn test_empty_content() {
        let matches = search_slice("foo", b"", &GrepOptions::default()).unwrap();
        assert_eq!(matches.len(), 0);
    }

    #[test]
    fn test_invalid_regex() {
        let result = search_slice("[invalid", b"content", &GrepOptions::default());
        assert!(result.is_err());
    }

    #[test]
    fn test_json_serialization() {
        let content = b"hello world\n";
        let matches = search_slice("hello", content, &GrepOptions::default()).unwrap();
        let json = serde_json::to_string(&matches).unwrap();
        assert!(json.contains("\"line_number\":1"));
        assert!(json.contains("\"line_text\":\"hello world\""));
    }
}
