// src/main.rs
use std::collections::HashMap;
use std::env;
use std::fs;
use std::io::{self, Write};

#[derive(Debug, Clone, PartialEq)]
enum Token {
    Integer(i32),
    Float(f64),
    Bool(bool),
    String(String),
    Symbol(String),
    ListStart,
    ListEnd,
    QuotationStart,
    QuotationEnd,
}

#[derive(Debug, Clone, PartialEq)]
enum Value {
    Integer(i32),
    Float(f64),
    Bool(bool),
    String(String),
    Symbol(String),
    List(Vec<Value>),
    Quotation(Vec<Value>),
}

#[derive(Debug)]
enum CompilerError {
    CompileError(String),
}

struct Compiler {
    functions: HashMap<String, Vec<Value>>,
}

impl Compiler {
    fn new() -> Self {
        Self {
            functions: HashMap::new(),
        }
    }

    fn parse(&mut self, input: &str) -> Result<Vec<Value>, CompilerError> {
        let tokens = self.tokenize(input)?;
        self.parse_tokens(&tokens, 0).map(|(values, _)| values)
    }

    fn tokenize(&self, input: &str) -> Result<Vec<Token>, CompilerError> {
        let mut tokens = Vec::new();
        let mut chars = input.chars().peekable();

        while let Some(c) = chars.next() {
            match c {
                // Skip whitespace
                c if c.is_whitespace() => continue,
                
                // Skip comments
                '#' => {
                    while let Some(c) = chars.peek() {
                        if *c == '\n' {
                            chars.next();
                            break;
                        }
                        chars.next();
                    }
                },
                
                // Parse strings
                '"' => {
                    let mut string = String::new();
                    while let Some(c) = chars.next() {
                        if c == '"' {
                            break;
                        }
                        string.push(c);
                    }
                    tokens.push(Token::String(string));
                },
                
                // Parse lists and quotations
                '[' => tokens.push(Token::ListStart),
                ']' => tokens.push(Token::ListEnd),
                '{' => tokens.push(Token::QuotationStart),
                '}' => tokens.push(Token::QuotationEnd),
                
                // Parse numbers, booleans and symbols
                _ => {
                    let mut token = String::new();
                    token.push(c);
                    
                    while let Some(&next_c) = chars.peek() {
                        if next_c.is_whitespace() || next_c == '[' || next_c == ']' || next_c == '{' || next_c == '}' {
                            break;
                        }
                        token.push(chars.next().unwrap());
                    }
                    
                    // Try to parse as integer
                    if let Ok(i) = token.parse::<i32>() {
                        tokens.push(Token::Integer(i));
                        continue;
                    }
                    
                    // Try to parse as float
                    if let Ok(f) = token.parse::<f64>() {
                        tokens.push(Token::Float(f));
                        continue;
                    }
                    
                    // Try to parse as boolean
                    match token.as_str() {
                        "True" => tokens.push(Token::Bool(true)),
                        "False" => tokens.push(Token::Bool(false)),
                        _ => tokens.push(Token::Symbol(token)),
                    }
                }
            }
        }
        
        Ok(tokens)
    }

    fn parse_tokens(&mut self, tokens: &[Token], pos: usize) -> Result<(Vec<Value>, usize), CompilerError> {
        let mut values = Vec::new();
        let mut i = pos;
        
        while i < tokens.len() {
            match &tokens[i] {
                Token::Integer(n) => values.push(Value::Integer(*n)),
                Token::Float(f) => values.push(Value::Float(*f)),
                Token::Bool(b) => values.push(Value::Bool(*b)),
                Token::String(s) => values.push(Value::String(s.clone())),
                Token::Symbol(s) => values.push(Value::Symbol(s.clone())),
                
                Token::ListStart => {
                    let (list_values, new_pos) = self.parse_tokens(tokens, i + 1)?;
                    values.push(Value::List(list_values));
                    i = new_pos;
                    continue;
                },
                
                Token::ListEnd => return Ok((values, i + 1)),
                
                Token::QuotationStart => {
                    let (quotation_values, new_pos) = self.parse_tokens(tokens, i + 1)?;
                    values.push(Value::Quotation(quotation_values));
                    i = new_pos;
                    continue;
                },
                
                Token::QuotationEnd => return Ok((values, i + 1)),
            }
            
            i += 1;
        }
        
        Ok((values, i))
    }

    fn generate_wat(&mut self, ast: &[Value]) -> Result<String, CompilerError> {
        // Scan for function definitions first
        self.extract_functions(ast)?;
        
        let mut output = String::new();
        
        // WAT module header
        output.push_str("(module\n");
        output.push_str("  ;; Import memory for string and list operations\n");
        output.push_str("  (memory (export \"memory\") 1)\n\n");
        output.push_str("  ;; Global stack pointer\n");
        output.push_str("  (global $sp (mut i32) (i32.const 0))\n\n");
        
        // Import JS console.log for print
        output.push_str("  ;; Import JavaScript functions\n");
        output.push_str("  (import \"console\" \"log\" (func $log (param i32)))\n");
        output.push_str("  (import \"env\" \"read_line\" (func $read_line (result i32)))\n\n");
        
        // Generate function definitions
        let functions = self.functions.clone(); // Clone to avoid borrowing conflicts
        for (name, body) in functions {
            output.push_str(&format!("  (func ${} (result i32)\n", name));
            output.push_str(&self.generate_function_body(&body)?);
            output.push_str("  )\n\n");
        }
        
        // Main function
        output.push_str("  (func $main (export \"main\") (result i32)\n");
        output.push_str("    ;; Local variables for stack operations\n");
        output.push_str("    (local $temp i32)\n");
        output.push_str("    (local $temp_a i32)\n");
        output.push_str("    (local $temp_b i32)\n\n");
        output.push_str(&self.generate_code(ast)?);
        output.push_str("  )\n");
        
        // Close the module
        output.push_str(")\n");
        
        Ok(output)
    }

    fn extract_functions(&mut self, ast: &[Value]) -> Result<(), CompilerError> {
        let mut i = 0;
        while i < ast.len() {
            if i + 2 < ast.len() {
                if let Value::Symbol(ref fun_name) = ast[i] {
                    if let Value::Quotation(ref body) = ast[i + 1] {
                        if let Value::Symbol(ref keyword) = ast[i + 2] {
                            if keyword == "fun" {
                                self.functions.insert(fun_name.clone(), body.clone());
                                i += 3;
                                continue;
                            }
                        }
                    }
                }
            }
            i += 1;
        }
        Ok(())
    }

    fn generate_function_body(&mut self, body: &[Value]) -> Result<String, CompilerError> {
        let mut code = String::new();
        code.push_str("    ;; Local variables for stack operations\n");
        code.push_str("    (local $temp i32)\n");
        code.push_str("    (local $temp_a i32)\n");
        code.push_str("    (local $temp_b i32)\n\n");
        code.push_str(&self.generate_code(body)?);
        Ok(code)
    }

    fn generate_code(&mut self, ast: &[Value]) -> Result<String, CompilerError> {
        let mut code = String::new();
        
        for value in ast {
            match value {
                Value::Integer(n) => {
                    code.push_str(&format!("    i32.const {}\n", n));
                },
                Value::Float(f) => {
                    code.push_str(&format!("    f32.const {}\n", f));
                },
                Value::Bool(b) => {
                    code.push_str(&format!("    i32.const {}\n", if *b { 1 } else { 0 }));
                },
                Value::String(s) => {
                    // This is a simplification - in a real compiler you'd need to
                    // allocate memory for strings and store them properly
                    code.push_str(&format!("    ;; String: {}\n", s));
                    // For now, we'll just push the string length as a placeholder
                    code.push_str(&format!("    i32.const {}\n", s.len()));
                },
                Value::Symbol(s) => {
                    match s.as_str() {
                        // Arithmetic operations
                        "+" => code.push_str("    i32.add\n"),
                        "-" => code.push_str("    i32.sub\n"),
                        "*" => code.push_str("    i32.mul\n"),
                        "div" => code.push_str("    i32.div_s\n"),
                        "/" => {
                            // Integer division, converts to float in real implementation
                            code.push_str("    i32.div_s\n");
                        },
                        
                        // Comparison operations
                        "==" => code.push_str("    i32.eq\n"),
                        "<" => code.push_str("    i32.lt_s\n"),
                        ">" => code.push_str("    i32.gt_s\n"),
                        
                        // Boolean operations
                        "&&" => {
                            code.push_str("    ;; Logical AND\n");
                            code.push_str("    i32.and\n");
                        },
                        "||" => {
                            code.push_str("    ;; Logical OR\n");
                            code.push_str("    i32.or\n");
                        },
                        "not" => {
                            code.push_str("    ;; Logical NOT (for booleans)\n");
                            code.push_str("    i32.const 1\n");
                            code.push_str("    i32.xor\n");
                        },
                        
                        // Stack operations
                        "dup" => {
                            code.push_str("    ;; Duplicate top of stack\n");
                            code.push_str("    local.set $temp\n");
                            code.push_str("    local.get $temp\n");
                            code.push_str("    local.get $temp\n");
                        },
                        "swap" => {
                            code.push_str("    ;; Swap top two values on stack\n");
                            code.push_str("    local.set $temp_b\n");
                            code.push_str("    local.set $temp_a\n");
                            code.push_str("    local.get $temp_b\n");
                            code.push_str("    local.get $temp_a\n");
                        },
                        "pop" => {
                            code.push_str("    ;; Pop top of stack\n");
                            code.push_str("    drop\n");
                        },
                        
                        // I/O operations
                        "print" => {
                            code.push_str("    ;; Print top of stack\n");
                            code.push_str("    call $log\n");
                        },
                        "read" => {
                            code.push_str("    ;; Read from stdin\n");
                            code.push_str("    call $read_line\n");
                        },
                        
                        // Function definition
                        "fun" => {
                            // This is handled at a higher level during function extraction
                            // Do nothing at code generation time
                            code.push_str("    ;; Function definition (handled at extraction phase)\n");
                        },
                        
                        // Function calls
                        _ => {
                            if self.functions.contains_key(s) {
                                code.push_str(&format!("    call ${}\n", s));
                            } else {
                                return Err(CompilerError::CompileError(format!("Unknown symbol: {}", s)));
                            }
                        }
                    }
                },
                Value::List(_) => {
                    // Simplification - lists would require memory allocation and management
                    code.push_str("    ;; List operations not implemented in this simple version\n");
                    code.push_str("    i32.const 0  ;; Placeholder for list\n");
                },
                Value::Quotation(q) => {
                    // For this simple version, we'll just inline the quotation
                    // In a real implementation, you'd create a function for each quotation
                    code.push_str("    ;; Begin quotation\n");
                    code.push_str(&self.generate_code(q)?);
                    code.push_str("    ;; End quotation\n");
                }
            }
        }
        
        // Ensure there's at least one value on the stack to return
        code.push_str("    ;; Ensure there's a return value (default to 0 if stack is empty)\n");
        code.push_str("    (if (result i32)\n");
        code.push_str("      (i32.eq (global.get $sp) (i32.const 0))\n");
        code.push_str("      (then (i32.const 0))\n");
        code.push_str("      (else\n");
        code.push_str("        ;; Return the top value from the stack\n");
        code.push_str("        ;; In a real implementation, we would pop it from a proper stack\n");
        code.push_str("        ;; For now, we'll just return whatever is on top of the WebAssembly stack\n");
        code.push_str("      )\n");
        code.push_str("    )\n");
        
        Ok(code)
    }

    fn compile(&mut self, input: &str) -> Result<String, CompilerError> {
        let ast = self.parse(input)?;
        self.generate_wat(&ast)
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let mut compiler = Compiler::new();
    
    if args.len() <= 1 {
        // REPL mode
        println!("BProg WAT compiler (REPL mode)");
        println!("Type a BProg expression, or 'exit' to quit");
        
        let stdin = io::stdin();
        let mut stdout = io::stdout();
        
        loop {
            print!("> ");
            stdout.flush().unwrap();
            
            let mut input = String::new();
            stdin.read_line(&mut input).unwrap();
            
            let input = input.trim();
            if input == "exit" {
                break;
            }
            
            match compiler.compile(input) {
                Ok(wat) => println!("{}", wat),
                Err(err) => eprintln!("Error: {:?}", err),
            }
        }
    } else {
        // File mode
        let input_file = &args[1];
        let output_file = if args.len() > 2 {
            args[2].clone()
        } else {
            let mut output = input_file.to_string();
            if output.ends_with(".bprog") {
                output = output.trim_end_matches(".bprog").to_string();
            }
            output + ".wat"
        };
        
        match fs::read_to_string(input_file) {
            Ok(content) => {
                match compiler.compile(&content) {
                    Ok(wat) => {
                        match fs::write(&output_file, wat) {
                            Ok(_) => println!("Successfully compiled {} to {}", input_file, output_file),
                            Err(err) => eprintln!("Error writing output file: {}", err),
                        }
                    },
                    Err(err) => eprintln!("Compilation error: {:?}", err),
                }
            },
            Err(err) => eprintln!("Error reading input file: {}", err),
        }
    }
}