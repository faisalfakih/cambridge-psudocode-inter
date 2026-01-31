use std::error::Error;
use colored::Colorize;


#[derive(Debug, Clone)]
pub enum ErrorType {
    Lexical, 
    Syntax,
    Semantic,
    Runtime,
}

#[derive(Debug, Clone)]
pub struct CPSError {
    pub error_type: ErrorType,
    pub message: String,
    pub hint : Option<String>,
    pub line: usize,
    pub column: usize,
    pub source: Option<String>,
}

impl Error for CPSError {}

impl std::fmt::Display for ErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ErrorType::Lexical => write!(f, "Lexical"),
            ErrorType::Syntax => write!(f, "Syntax"),
            ErrorType::Semantic => write!(f, "Semantic"),
            ErrorType::Runtime => write!(f, "Runtime")
        }
    }
}
    


impl std::fmt::Display for CPSError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let _ = writeln!(f, "{}: {} Error at line {}, column {}: {}",
            "ERROR".bright_red().bold(),
            format!("{:?}", self.error_type).bright_red(),
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
