// src/main.rs
use std::collections::HashMap;
use std::env;
use std::fs;
use std::io::{self, BufRead, Write};
use std::process;

#[derive(Debug, Clone, PartialEq)]
enum Value {
    Integer(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Symbol(String),
    List(Vec<Value>),
    Quotation(Vec<Value>),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Integer(i) => write!(f, "{}", i),
            Value::Float(fl) => write!(f, "{}", fl),
            Value::Bool(b) => write!(f, "{}", b),
            Value::String(s) => write!(f, "\"{}\"", s),
            Value::Symbol(s) => write!(f, "{}", s),
            Value::List(l) => {
                write!(f, "[")?;
                for (i, v) in l.iter().enumerate() {
                    if i > 0 {
                        write!(f, ",")?;
                    }
                    write!(f, "{}", v)?;
                }
                write!(f, "]")
            }
            Value::Quotation(q) => {
                write!(f, "{{ ")?;
                for v in q {
                    write!(f, "{} ", v)?;
                }
                write!(f, "}}")
            }
        }
    }
}

#[derive(Debug)]
enum ProgramError {
    StackEmpty,
    ExpectedBool,
    ExpectedNumber,
    ExpectedQuotation,
    ExpectedList,
    ExpectedSymbol,
    DivisionByZero,
    ProgramFinishedWithMultipleValues,
    ProgramFinishedWithNoValues,
    NumberConversionError,
    ParseError(String),
}

impl std::fmt::Display for ProgramError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ProgramError::StackEmpty => write!(f, "Error: Stack is empty"),
            ProgramError::ExpectedBool => write!(f, "Error: Expected a boolean value"),
            ProgramError::ExpectedNumber => write!(f, "Error: Expected a number"),
            ProgramError::ExpectedQuotation => write!(f, "Error: Expected a quotation"),
            ProgramError::ExpectedList => write!(f, "Error: Expected a list"),
            ProgramError::ExpectedSymbol => write!(f, "Error: Expected a symbol"),
            ProgramError::DivisionByZero => write!(f, "Error: Division by zero"),
            ProgramError::ProgramFinishedWithMultipleValues => {
                write!(f, "Error: Program finished with multiple values on the stack")
            }
            ProgramError::ProgramFinishedWithNoValues => {
                write!(f, "Error: Program finished with no values on the stack")
            }
            ProgramError::NumberConversionError => {
                write!(f, "Error: Could not convert string to number")
            }
            ProgramError::ParseError(s) => write!(f, "Parse error: {}", s),
        }
    }
}

struct Interpreter {
    stack: Vec<Value>,
    dictionary: HashMap<String, Value>,
}

impl Interpreter {
    fn new() -> Self {
        Self {
            stack: Vec::new(),
            dictionary: HashMap::new(),
        }
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn pop(&mut self) -> Result<Value, ProgramError> {
        self.stack.pop().ok_or(ProgramError::StackEmpty)
    }

    fn parse_tokens(&self, input: &str) -> Result<Vec<Value>, ProgramError> {
    let mut tokens = Vec::new();
    
    // Process the input line by line to handle comments
    let filtered_lines: Vec<String> = input
        .lines()
        .map(|line| {
            if let Some(index) = line.find('#') {
                // If the line contains a #, only take the part before it
                line[0..index].to_string()
            } else {
                // Otherwise, use the whole line
                line.to_string()
            }
        })
        .collect();
    
    let filtered_input = filtered_lines.join("\n");
    
    // Now process the filtered input with your existing tokenization logic
    let mut i = 0;
    let chars: Vec<char> = filtered_input.chars().collect();

    while i < chars.len() {
        if chars[i].is_whitespace() {
            i += 1;
            continue;
        }

        // Parse string
        if chars[i] == '"' {
            let mut string = String::new();
            i += 1; // Skip opening quote
            while i < chars.len() && chars[i] != '"' {
                string.push(chars[i]);
                i += 1;
            }
            if i >= chars.len() {
                return Err(ProgramError::ParseError("Unterminated string".to_string()));
            }
            i += 1; // Skip closing quote
            tokens.push(Value::String(string));
            continue;
        }

        // Parse list
        if chars[i] == '[' {
            let mut depth = 1;
            let start = i + 1;
            i += 1;
            while i < chars.len() && depth > 0 {
                if chars[i] == '[' {
                    depth += 1;
                } else if chars[i] == ']' {
                    depth -= 1;
                }
                i += 1;
            }
            if depth > 0 {
                return Err(ProgramError::ParseError("Unterminated list".to_string()));
            }
            let list_content = &filtered_input[start..i - 1];
            let list_values = self.parse_tokens(list_content)?;
            tokens.push(Value::List(list_values));
            continue;
        }

        // Parse quotation
        if chars[i] == '{' {
            let mut depth = 1;
            let start = i + 1;
            i += 1;
            while i < chars.len() && depth > 0 {
                if chars[i] == '{' {
                    depth += 1;
                } else if chars[i] == '}' {
                    depth -= 1;
                }
                i += 1;
            }
            if depth > 0 {
                return Err(ProgramError::ParseError("Unterminated quotation".to_string()));
            }
            let quotation_content = &filtered_input[start..i - 1];
            let quotation_values = self.parse_tokens(quotation_content)?;
            tokens.push(Value::Quotation(quotation_values));
            continue;
        }

        // Parse token (number, bool, or symbol)
        let mut token = String::new();
        while i < chars.len() && !chars[i].is_whitespace() && !matches!(chars[i], '"' | '[' | ']' | '{' | '}') {
            token.push(chars[i]);
            i += 1;
        }

        // Clean up any stray carriage-returns or BOMs
        let token = token.trim_matches(|c: char| c == '\r' || c == '\u{feff}').to_string();

        // Skip empty tokens
        if token.is_empty() {
            continue;
        }

        // Try to parse as integer
        if let Ok(int_val) = token.parse::<i64>() {
            tokens.push(Value::Integer(int_val));
            continue;
        }

        // Try to parse as float
        if let Ok(float_val) = token.parse::<f64>() {
            tokens.push(Value::Float(float_val));
            continue;
        }

        // Try to parse as bool
        match token.as_str() {
            "True" => tokens.push(Value::Bool(true)),
            "False" => tokens.push(Value::Bool(false)),
            _ => tokens.push(Value::Symbol(token)),
        }
    }

    Ok(tokens)
}

    fn interpret(&mut self, tokens: &[Value]) -> Result<(), ProgramError> {
        let mut i = 0;
        while i < tokens.len() {
            match &tokens[i] {
                Value::Symbol(symbol) => {
                    match symbol.as_str() {
                        // Stack operations
                        "dup" => {
                            let value = self.pop()?;
                            self.push(value.clone());
                            self.push(value);
                        }
                        "swap" => {
                            let b = self.pop()?;
                            let a = self.pop()?;
                            self.push(b);
                            self.push(a);
                        }
                        "pop" => {
                            self.pop()?;
                        }
                        // Arithmetic operations
                        "+" => {
                            let b = self.pop()?;
                            let a = self.pop()?;
                            match (a, b) {
                                (Value::Integer(a_int), Value::Integer(b_int)) => {
                                    self.push(Value::Integer(a_int + b_int));
                                }
                                (Value::Float(a_float), Value::Float(b_float)) => {
                                    self.push(Value::Float(a_float + b_float));
                                }
                                (Value::Integer(a_int), Value::Float(b_float)) => {
                                    self.push(Value::Float(a_int as f64 + b_float));
                                }
                                (Value::Float(a_float), Value::Integer(b_int)) => {
                                    self.push(Value::Float(a_float + b_int as f64));
                                }
                                _ => return Err(ProgramError::ExpectedNumber),
                            }
                        }
                        "-" => {
                            let b = self.pop()?;
                            let a = self.pop()?;
                            match (a, b) {
                                (Value::Integer(a_int), Value::Integer(b_int)) => {
                                    self.push(Value::Integer(a_int - b_int));
                                }
                                (Value::Float(a_float), Value::Float(b_float)) => {
                                    self.push(Value::Float(a_float - b_float));
                                }
                                (Value::Integer(a_int), Value::Float(b_float)) => {
                                    self.push(Value::Float(a_int as f64 - b_float));
                                }
                                (Value::Float(a_float), Value::Integer(b_int)) => {
                                    self.push(Value::Float(a_float - b_int as f64));
                                }
                                _ => return Err(ProgramError::ExpectedNumber),
                            }
                        }
                        "*" => {
                            let b = self.pop()?;
                            let a = self.pop()?;
                            match (a, b) {
                                (Value::Integer(a_int), Value::Integer(b_int)) => {
                                    self.push(Value::Integer(a_int * b_int));
                                }
                                (Value::Float(a_float), Value::Float(b_float)) => {
                                    self.push(Value::Float(a_float * b_float));
                                }
                                (Value::Integer(a_int), Value::Float(b_float)) => {
                                    self.push(Value::Float(a_int as f64 * b_float));
                                }
                                (Value::Float(a_float), Value::Integer(b_int)) => {
                                    self.push(Value::Float(a_float * b_int as f64));
                                }
                                _ => return Err(ProgramError::ExpectedNumber),
                            }
                        }
                        "/" => {
                            let b = self.pop()?;
                            let a = self.pop()?;
                            match (a, b) {
                                (Value::Integer(a_int), Value::Integer(b_int)) => {
                                    if b_int == 0 {
                                        return Err(ProgramError::DivisionByZero);
                                    }
                                    self.push(Value::Float(a_int as f64 / b_int as f64));
                                }
                                (Value::Float(a_float), Value::Float(b_float)) => {
                                    if b_float == 0.0 {
                                        return Err(ProgramError::DivisionByZero);
                                    }
                                    self.push(Value::Float(a_float / b_float));
                                }
                                (Value::Integer(a_int), Value::Float(b_float)) => {
                                    if b_float == 0.0 {
                                        return Err(ProgramError::DivisionByZero);
                                    }
                                    self.push(Value::Float(a_int as f64 / b_float));
                                }
                                (Value::Float(a_float), Value::Integer(b_int)) => {
                                    if b_int == 0 {
                                        return Err(ProgramError::DivisionByZero);
                                    }
                                    self.push(Value::Float(a_float / b_int as f64));
                                }
                                _ => return Err(ProgramError::ExpectedNumber),
                            }
                        }
                        "div" => {
                            let b = self.pop()?;
                            let a = self.pop()?;
                            match (a, b) {
                                (Value::Integer(a_int), Value::Integer(b_int)) => {
                                    if b_int == 0 {
                                        return Err(ProgramError::DivisionByZero);
                                    }
                                    self.push(Value::Integer(a_int / b_int));
                                }
                                (Value::Float(a_float), Value::Float(b_float)) => {
                                    if b_float == 0.0 {
                                        return Err(ProgramError::DivisionByZero);
                                    }
                                    self.push(Value::Integer(a_float as i64 / b_float as i64));
                                }
                                (Value::Integer(a_int), Value::Float(b_float)) => {
                                    if b_float == 0.0 {
                                        return Err(ProgramError::DivisionByZero);
                                    }
                                    self.push(Value::Integer(a_int / b_float as i64));
                                }
                                (Value::Float(a_float), Value::Integer(b_int)) => {
                                    if b_int == 0 {
                                        return Err(ProgramError::DivisionByZero);
                                    }
                                    self.push(Value::Integer(a_float as i64 / b_int));
                                }
                                _ => return Err(ProgramError::ExpectedNumber),
                            }
                        }
                        "<" => {
                            let b = self.pop()?;
                            let a = self.pop()?;
                            match (a, b) {
                                (Value::Integer(a_int), Value::Integer(b_int)) => {
                                    self.push(Value::Bool(a_int < b_int));
                                }
                                (Value::Float(a_float), Value::Float(b_float)) => {
                                    self.push(Value::Bool(a_float < b_float));
                                }
                                (Value::Integer(a_int), Value::Float(b_float)) => {
                                    self.push(Value::Bool((a_int as f64) < b_float));
                                }
                                (Value::Float(a_float), Value::Integer(b_int)) => {
                                    self.push(Value::Bool(a_float < (b_int as f64)));
                                }
                                _ => return Err(ProgramError::ExpectedNumber),
                            }
                        }
                        ">" => {
                            let b = self.pop()?;
                            let a = self.pop()?;
                            match (a, b) {
                                (Value::Integer(a_int), Value::Integer(b_int)) => {
                                    self.push(Value::Bool(a_int > b_int));
                                }
                                (Value::Float(a_float), Value::Float(b_float)) => {
                                    self.push(Value::Bool(a_float > b_float));
                                }
                                (Value::Integer(a_int), Value::Float(b_float)) => {
                                    self.push(Value::Bool((a_int as f64) > b_float));
                                }
                                (Value::Float(a_float), Value::Integer(b_int)) => {
                                    self.push(Value::Bool(a_float > (b_int as f64)));
                                }
                                _ => return Err(ProgramError::ExpectedNumber),
                            }
                        }
                        "==" => {
                            let b = self.pop()?;
                            let a = self.pop()?;
                            self.push(Value::Bool(a == b));
                        }
                        // Logic operations
                        "&&" => {
                            let b = self.pop()?;
                            let a = self.pop()?;
                            match (a, b) {
                                (Value::Bool(a_bool), Value::Bool(b_bool)) => {
                                    self.push(Value::Bool(a_bool && b_bool));
                                }
                                _ => return Err(ProgramError::ExpectedBool),
                            }
                        }
                        "||" => {
                            let b = self.pop()?;
                            let a = self.pop()?;
                            match (a, b) {
                                (Value::Bool(a_bool), Value::Bool(b_bool)) => {
                                    self.push(Value::Bool(a_bool || b_bool));
                                }
                                _ => return Err(ProgramError::ExpectedBool),
                            }
                        }
                        "not" => {
                            let value = self.pop()?;
                            match value {
                                Value::Bool(b) => self.push(Value::Bool(!b)),
                                Value::Integer(n) => self.push(Value::Integer(-n)),
                                Value::Float(f) => self.push(Value::Float(-f)),
                                _ => return Err(ProgramError::ExpectedBool),
                            }
                        }
                        // String parsing
                        "parseInteger" => {
                            let value = self.pop()?;
                            match value {
                                Value::String(s) => {
                                    if let Ok(int_val) = s.trim().parse::<i64>() {
                                        self.push(Value::Integer(int_val));
                                    } else {
                                        return Err(ProgramError::NumberConversionError);
                                    }
                                }
                                _ => return Err(ProgramError::ExpectedSymbol),
                            }
                        }
                        "parseFloat" => {
                            let value = self.pop()?;
                            match value {
                                Value::String(s) => {
                                    if let Ok(float_val) = s.trim().parse::<f64>() {
                                        self.push(Value::Float(float_val));
                                    } else {
                                        return Err(ProgramError::NumberConversionError);
                                    }
                                }
                                _ => return Err(ProgramError::ExpectedSymbol),
                            }
                        }
                        "words" => {
                            let value = self.pop()?;
                            match value {
                                Value::String(s) => {
                                    let words: Vec<Value> = s
                                        .split_whitespace()
                                        .map(|word| Value::String(word.to_string()))
                                        .collect();
                                    self.push(Value::List(words));
                                }
                                _ => return Err(ProgramError::ExpectedSymbol),
                            }
                        }
                        // List operations
                        "head" => {
                            let value = self.pop()?;
                            match value {
                                Value::List(list) => {
                                    if list.is_empty() {
                                        return Err(ProgramError::ParseError("Empty list has no head".to_string()));
                                    }
                                    self.push(list[0].clone());
                                }
                                _ => return Err(ProgramError::ExpectedList),
                            }
                        }
                        "tail" => {
                            let value = self.pop()?;
                            match value {
                                Value::List(list) => {
                                    if list.is_empty() {
                                        self.push(Value::List(vec![]));
                                    } else {
                                        self.push(Value::List(list[1..].to_vec()));
                                    }
                                }
                                _ => return Err(ProgramError::ExpectedList),
                            }
                        }
                        "empty" => {
                            let value = self.pop()?;
                            match value {
                                Value::List(list) => {
                                    self.push(Value::Bool(list.is_empty()));
                                }
                                _ => return Err(ProgramError::ExpectedList),
                            }
                        }
                        "length" => {
                            let value = self.pop()?;
                            match &value {
                                Value::List(list) => {
                                    self.push(Value::Integer(list.len() as i64));
                                }
                                Value::String(s) => {
                                    self.push(Value::Integer(s.len() as i64));
                                }
                                Value::Quotation(q) => {
                                    self.push(Value::Integer(q.len() as i64));
                                }
                                _ => return Err(ProgramError::ParseError(format!("Cannot get length of {:?}", value))),
                            }
                        }
                        "cons" => {
                            let list_val = self.pop()?;
                            let item = self.pop()?;
                            match list_val {
                                Value::List(list) => {
                                    let mut new_list = vec![item];
                                    new_list.extend(list);
                                    self.push(Value::List(new_list));
                                }
                                _ => return Err(ProgramError::ExpectedList),
                            }
                        }
                        "append" => {
                            let list2 = self.pop()?;
                            let list1 = self.pop()?;
                            match (list1, list2) {
                                (Value::List(mut l1), Value::List(l2)) => {
                                    l1.extend(l2);
                                    self.push(Value::List(l1));
                                }
                                _ => return Err(ProgramError::ExpectedList),
                            }
                        }
                        "each" => {
                            let block = self.pop()?;
                            let list_val = self.pop()?;
                            
                            let block_values = match block {
                                Value::Quotation(q) => q,
                                _ => return Err(ProgramError::ExpectedQuotation),
                            };
                            
                            match list_val {
                                Value::List(list) => {
                                    for item in list {
                                        self.push(item);
                                        self.interpret(&block_values)?;
                                    }
                                }
                                _ => return Err(ProgramError::ExpectedList),
                            }
                        }
                        "map" => {
                            let block = self.pop()?;
                            let list_val = self.pop()?;
                            
                            let block_values = match block {
                                Value::Quotation(q) => q,
                                _ => return Err(ProgramError::ExpectedQuotation),
                            };
                            
                            match list_val {
                                Value::List(list) => {
                                    let mut result = Vec::new();
                                    for item in list {
                                        self.push(item);
                                        self.interpret(&block_values)?;
                                        result.push(self.pop()?);
                                    }
                                    self.push(Value::List(result));
                                }
                                _ => return Err(ProgramError::ExpectedList),
                            }
                        }
                        "foldl" => {
                            let block = self.pop()?;
                            let initial = self.pop()?;
                            let list_val = self.pop()?;
                            
                            let block_values = match block {
                                Value::Quotation(q) => q,
                                _ => return Err(ProgramError::ExpectedQuotation),
                            };
                            
                            match list_val {
                                Value::List(list) => {
                                    self.push(initial);
                                    for item in list {
                                        self.push(item);
                                        self.interpret(&block_values)?;
                                    }
                                }
                                _ => return Err(ProgramError::ExpectedList),
                            }
                        }
                        // Control flow
                        "if" => {
                            let condition = self.pop()?;
                            
                            // Get then_block and else_block
                            if i + 2 >= tokens.len() {
                                return Err(ProgramError::ParseError("if requires then and else blocks".to_string()));
                            }
                            
                            let then_block = match &tokens[i + 1] {
                                Value::Quotation(q) => q.clone(),
                                val => vec![val.clone()], // Single value case for ergonomics
                            };
                            
                            let else_block = match &tokens[i + 2] {
                                Value::Quotation(q) => q.clone(),
                                val => vec![val.clone()], // Single value case for ergonomics
                            };
                            
                            match condition {
                                Value::Bool(true) => self.interpret(&then_block)?,
                                Value::Bool(false) => self.interpret(&else_block)?,
                                _ => return Err(ProgramError::ExpectedBool),
                            }
                            
                            i += 2; // Skip the then and else blocks
                        }
                        "times" => {
                            let block = self.pop()?;
                            let count_val = self.pop()?;
                            
                            let block_values = match block {
                                Value::Quotation(q) => q,
                                val => vec![val], // Single value case for ergonomics
                            };
                            
                            let count = match count_val {
                                Value::Integer(n) => n,
                                _ => return Err(ProgramError::ExpectedNumber),
                            };
                            
                            for _ in 0..count {
                                self.interpret(&block_values)?;
                            }
                        }
                        "loop" => {
                            let block = self.pop()?;
                            let break_cond = self.pop()?;
                            
                            let break_values = match break_cond {
                                Value::Quotation(q) => q,
                                _ => return Err(ProgramError::ExpectedQuotation),
                            };
                            
                            let block_values = match block {
                                Value::Quotation(q) => q,
                                _ => return Err(ProgramError::ExpectedQuotation),
                            };
                            
                            loop {
                                // Check break condition
                                self.interpret(&break_values)?;
                                let should_break = match self.pop()? {
                                    Value::Bool(b) => b,
                                    _ => return Err(ProgramError::ExpectedBool),
                                };
                                
                                if should_break {
                                    break;
                                }
                                
                                // Execute loop body
                                self.interpret(&block_values)?;
                            }
                        }
                        // Quotation execution
                        "exec" => {
                            let quotation = self.pop()?;
                            match quotation {
                                Value::Quotation(q) => {
                                    self.interpret(&q)?;
                                }
                                _ => return Err(ProgramError::ExpectedQuotation),
                            }
                        }
                        // Variable and function definition
                        ":=" => {
                            let value = self.pop()?;
                            let name = self.pop()?;
                            match name {
                                Value::Symbol(s) => {
                                    self.dictionary.insert(s, value);
                                }
                                _ => return Err(ProgramError::ExpectedSymbol),
                            }
                        }
                        "fun" => {
                            let block = self.pop()?;
                            let name = self.pop()?;
                            match (name, block) {
                                (Value::Symbol(s), Value::Quotation(q)) => {
                                    self.dictionary.insert(s.clone(), Value::Quotation(q));
                                    eprintln!("Debug: Added function '{}' to dictionary", s);
                                }
                                _ => return Err(ProgramError::ExpectedSymbol),
                            }
                        }
                        // I/O operations
                        "print" => {
                            let value = self.pop()?;
                            println!("{}", value);
                        }
                        "read" => {
                            let mut input = String::new();
                            io::stdin().read_line(&mut input).expect("Failed to read line");
                            self.push(Value::String(input.trim().to_string()));
                        }
                        // If it's not a built-in, check if it's in the dictionary
_ => {
    // Clone the symbol to avoid borrowing conflicts
    let symbol_clone = symbol.clone();
    
    // Look up in the dictionary
    if let Some(value) = self.dictionary.get(&symbol_clone) {
        match value {
            Value::Quotation(q) => {
                // Clone the quotation to avoid borrowing issues
                let q_clone = q.clone();
                self.interpret(&q_clone)?;
            }
            _ => {
                self.push(value.clone());
            }
        }
    } else {
        // If not in the dictionary, push the symbol itself
        // Clone it again because push will consume it
        self.push(Value::Symbol(symbol_clone.clone()));
        
        // Now we can use symbol_clone for debug logging
        eprintln!("Warning: Unknown symbol '{}' - pushed as is", symbol_clone);
    }
}
                    }
                }
                // For non-symbol tokens, just push them onto the stack
                _ => {
                    self.push(tokens[i].clone());
                }
            }
            i += 1;
        }
        
        Ok(())
    }

    fn execute(&mut self, input: &str, is_repl: bool) -> Result<Option<Value>, ProgramError> {
        // Trim the input to remove any leading/trailing whitespace or special characters
        let trimmed_input = input.trim();
        
        // Parse and execute the tokens
        let tokens = self.parse_tokens(trimmed_input)?;
        
        self.interpret(&tokens)?;
        
        if is_repl {
            // In REPL mode, just return the top of the stack if it exists
            if !self.stack.is_empty() {
                Ok(Some(self.stack.last().unwrap().clone()))
            } else {
                Ok(None)
            }
        } else {
            // In normal mode, we expect a single value on the stack
            if self.stack.is_empty() {
                Err(ProgramError::ProgramFinishedWithNoValues)
            } else if self.stack.len() > 1 {
                Err(ProgramError::ProgramFinishedWithMultipleValues)
            } else {
                Ok(Some(self.pop()?))
            }
        }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let mut interpreter = Interpreter::new();
    
    // Try to load prelude if it exists
    if let Ok(prelude) = fs::read_to_string("prelude.bprog") {
        // Trim the prelude content to avoid any whitespace/special character issues
        let trimmed_prelude = prelude.trim();
        if let Err(err) = interpreter.execute(trimmed_prelude, true) {
            eprintln!("Error in prelude: {}", err);
            // Continue execution even if prelude has an error
        }
        
        // Clear the stack after loading the prelude
        interpreter.stack.clear();
    }
    
    if args.len() > 1 {
        // Run file mode
        match fs::read_to_string(&args[1]) {
            Ok(content) => {
                // Trim the file content to avoid any whitespace/special character issues
                let trimmed_content = content.trim();
                match interpreter.execute(trimmed_content, false) {
                    Ok(Some(result)) => println!("{}", result),
                    Err(err) => {
                        eprintln!("Error: {}", err);
                        process::exit(1);
                    },
                    _ => {}
                }
            }
            Err(err) => {
                eprintln!("Error reading file: {}", err);
                process::exit(1);
            }
        }
    } else {
        // REPL mode
        println!("BProg interpreter (REPL mode)");
        println!("Type 'exit' to quit");
        
        let stdin = io::stdin();
        let mut stdout = io::stdout();
        
        loop {
            print!("> ");
            stdout.flush().unwrap();
            
            let mut input = String::new();
            stdin.lock().read_line(&mut input).unwrap();
            
            let input = input.trim();
            if input == "exit" {
                break;
            }
            
            match interpreter.execute(input, true) {
                Ok(Some(result)) => println!("{}", result),
                Ok(None) => println!(""),
                Err(err) => eprintln!("Error: {}", err),
            }
        }
    }
}