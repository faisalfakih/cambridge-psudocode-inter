# Cambridge Pseudocode Interpreter

A Rust-based interpreter for Cambridge International AS & A Level Computer Science (9618) pseudocode specification. This project implements a complete interpreter capable of parsing and executing pseudocode according to the Cambridge syllabus standards.

## Project Overview

This interpreter is designed to help students and educators work with Cambridge 9618 pseudocode by providing a fully functional execution environment. It handles the complete pseudocode syntax including variable declarations, control structures, expressions, and I/O operations.

## Features

### Supported Language Features

- **Data Types**
  - `INTEGER` - Whole numbers
  - `REAL` - Floating-point numbers
  - `STRING` - Text data
  - `CHAR` - Single characters
  - `BOOLEAN` - True/False values

- **Variable Operations**
  - `DECLARE` statements with type annotations
  - Assignment using `<-` operator
  - Automatic type conversion (Real ↔ Integer where appropriate)
  - Proper variable scoping

- **Control Structures**
  - `IF...THEN...ELSE...ENDIF` conditionals
  - `WHILE...ENDWHILE` loops
  - `FOR...TO...NEXT` loops with integer iteration

- **Operators**
  - Arithmetic: `+`, `-`, `*`, `/`, `DIV`, `MOD`, `^` (power)
  - Comparison: `=`, `<>`, `<`, `<=`, `>`, `>=`
  - Logical: `AND`, `OR`
  - String concatenation: `&`

- **I/O Operations**
  - `OUTPUT` - Display values to console
  - `INPUT` - Read user input with type-aware parsing

## 💡 Motivation

The best way to learn programming is through hands-on practice—writing code, testing it, debugging it, and iterating. However, for students studying Cambridge Computer Science, the lack of a reliable, freely available pseudocode interpreter has been a significant barrier. Without a way to execute their pseudocode, learners are forced to manually trace through logic or rely on expensive proprietary tools, which limits experimentation and slows down the learning process. This project aims to remove that barrier by providing an accessible, open-source interpreter that empowers students to learn through doing—running their code, seeing immediate results, and developing a deeper intuition for programming concepts.

As a student working with the Cambridge International curriculum myself, I experienced this gap firsthand. This interpreter was born from both personal need and a desire to give back to the community:

- **Immediate feedback** - Run and test pseudocode without manual execution
- **Learning by doing** - Enable experimentation and iterative development
- **Community resource** - Provide a free tool for fellow students and educators worldwide
- **Skill demonstration** - Showcase compiler theory, type systems, and Rust proficiency

Building this project has deepened my understanding of:
- Lexical analysis and tokenization
- Parsing algorithms (specifically Pratt parsing)
- Type systems and runtime environments
- Error handling and user experience in developer tools

## 🏗️ Architecture

The interpreter follows a classic three-stage architecture:

```
Source Code → Lexer → Parser → Interpreter → Output
```

### Components

1. **Lexer** (`Lexer/lexer.rs`)
   - Tokenizes source code into meaningful tokens
   - Handles keywords, operators, literals, and identifiers

2. **Parser** (`Parser/parser.rs`, `Parser/ast.rs`)
   - Implements Pratt parsing for expressions
   - Builds Abstract Syntax Tree (AST)
   - Validates syntax according to Cambridge specification

3. **Interpreter** (`Inter/interpreter.rs`, `Inter/cps.rs`)
   - Evaluates AST nodes
   - Manages runtime environment and variable storage
   - Performs type checking and conversion
   - Executes control flow structures

4. **Error Handling** (`errortype.rs`)
   - Comprehensive error reporting with line/column information
   - Helpful hints for common mistakes
   - Runtime and syntax error differentiation

## Getting Started

### Prerequisites

- Rust 1.70 or higher
- Cargo (comes with Rust)

### Installation

```bash
# Clone the repository
git clone https://github.com/faisalfakih/cambridge-pseudocode-interpreter.git
cd cambridge-pseudocode-interpreter

# Build the project
cargo build --release
```

### Usage

```bash
# Run the interpreter
cargo run

# Or run the compiled binary
./target/release/cambridge-pseudocode-interpreter
```

### Example Program

```pseudocode
DECLARE Value : INTEGER
DECLARE Counter : INTEGER

Value <- 10

FOR Counter <- 1 TO Value
    OUTPUT Counter
NEXT Counter

OUTPUT "Loop complete!"
```

## Language Syntax

### Variable Declaration
```pseudocode
DECLARE variableName : TYPE
```

### Assignment
```pseudocode
variableName <- expression
```

### Conditional Statements
```pseudocode
IF condition THEN
    // statements
ELSE
    // statements
ENDIF
```

### Loops
```pseudocode
// While loop
WHILE condition
    // statements
ENDWHILE

// For loop
FOR counter <- start TO end
    // statements
NEXT counter
```

### Input/Output
```pseudocode
INPUT variableName
OUTPUT expression
```

## Technical Details

### Type System

The interpreter implements a strict type system with runtime type checking:

- All numeric literals are initially parsed as `Real`
- Type conversion occurs automatically during:
  - Variable assignment (Real → Integer or Integer → Real based on declared type)
  - Input operations (string input converted to declared variable type)
  - Arithmetic operations (mixed Integer/Real operations promote to Real)
  - This is mainly done for simplicity

### Expression Evaluation

- Uses Pratt parsing algorithm for operator precedence
- Supports nested expressions with parentheses
- Handles operator associativity (left/right)
- Precedence levels:
  - 30: `^` (power)
  - 20: `*`, `/`, `DIV`, `MOD`
  - 10: `+`, `-`
  - 8: `&` (concatenation)
  - 5: Comparison operators
  - 3: `AND`
  - 2: `OR`

### Environment Management

- Hierarchical environment structure supporting nested scopes
- Parent-child relationship for scope inheritance
- Variable lookup traverses scope chain
- Type information stored alongside values

## Testing

```bash
# Run all tests
cargo test

# Run with verbose output
cargo test -- --nocapture
```

## Current Status

### Implemented
- Complete lexer with all Cambridge pseudocode tokens
- Full expression parser with operator precedence
- Statement parsing (declarations, assignments, control structures)
- Runtime interpreter with type system
- Variable environment with scoping
- I/O operations
- All basic data types

### In Progress
- Array support
- Procedure and function definitions
- File I/O operations

### Planned
- Support for A-Level specific features

## Contributing

Contributions are welcome! This project is being developed as part of learning compiler/interpreter design and helping students with Cambridge Computer Science.

### Areas for Contribution
- Additional language features
- Test cases and examples
- Documentation improvements
- Bug fixes and optimizations
- Educational resources

## Resources

- [Cambridge Pseudocode Guide for Teachers](https://www.cambridgeinternational.org/Images/697401-2026-pseudocode-guide-for-teachers.pdf)

## License

This project is available for educational purposes. See LICENSE file for details.

## 🙏 Acknowledgments

- Built as a learning project for understanding interpreter design
- Inspired by the need for accessible Cambridge pseudocode tools
- Thanks to the Rust community for excellent documentation and libraries

## 📧 Contact

For questions, suggestions, or discussions about the project, feel free to open an issue on GitHub.

---

**Note**: This interpreter is an educational tool and may not cover every edge case in the Cambridge specification. Always refer to official Cambridge resources for authoritative information on pseudocode syntax and semantics.
