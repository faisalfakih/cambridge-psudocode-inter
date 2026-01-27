use std::{fmt, error::Error};
use colored::Colorize;

#[derive(Debug, Clone)]
pub struct LexicalError {
    pub message: String,
    pub hint : Option<String>,
    pub line: usize,
    pub column: usize,
    pub source: Option<String>,
}

impl Error for LexicalError {}
    


impl std::fmt::Display for LexicalError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let _ = writeln!(f, "{}: Lexical Error at line {}, column {}: {}",
            "ERROR".bright_red().bold(),
            self.line,
            self.column,
            self.message
        );
        if let Some(source) = &self.source {
            let lines: Vec<&str> = source.lines().collect();
            
            if self.line > 0 && self.line <= lines.len() {
                let error_line = lines[self.line - 1];
                let remaining = error_line.len() - (self.column - 1);
                let underline_length = remaining.min(error_line.len());
                
                writeln!(f, "{}", error_line)?;
                writeln!(f, "{}{}",
                    " ".repeat(self.column - 1),
                    "^".repeat(underline_length).bright_red().bold()
                )?;
            }
        }
        
        if let Some(hint) = &self.hint {
            write!(f, "{}: {}", "HINT".bright_yellow().bold(), hint)
        } else {
            Ok(())
        }
    }
}
