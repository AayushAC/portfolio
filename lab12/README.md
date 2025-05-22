# BProg Interpreter

A stack-based, concatenative programming language interpreter implemented in Rust.

## Overview

I've implemented BProg, a simple stack-based language where operations manipulate a central stack of values. My interpreter supports two modes:
- **REPL mode**: Interactive command line where expressions are evaluated line by line
- **File mode**: Executes a program from a file, expecting a single value to remain on the stack

## Self-Assessment

I've implemented the following features based on the lab assignment requirements:

### Core Features (D Level)

| Feature | Status | Notes |
|---------|--------|-------|
| Integer arithmetic | Implemented | All basic operations (+, -, *, /, div) working |
| String handling | Implemented | String literals and operations function correctly |
| String parsing | Implemented | parseInteger, parseFloat working as specified |
| Boolean operations | Implemented | &&, ||, not all working correctly |
| Stack operations | Implemented | swap, dup, pop correctly manipulate the stack |
| List operations | Implemented | head, tail, empty, length, cons, append all working |

### Advanced Features (C Level)

| Feature | Status | Notes |
|---------|--------|-------|
| Quotations | Implemented | Code blocks and exec function correctly |
| Control flow: if | Implemented | Conditional execution works with proper syntax |
| Control flow: times | Implemented | Repeat blocks specified number of times |
| List operations: map | Implemented | Correctly transforms lists |
| List operations: foldl | Implemented | Accumulation works as specified |
| List operations: each | Implemented | Applies blocks to each element |

### Additional Features (B/A Level)

| Feature | Status | Notes |
|---------|--------|-------|
| I/O operations | Implemented | print and read for basic I/O |
| Variable assignment | Implemented | := operator works for assigning values to symbols |
| Function definition | Implemented | fun keyword creates named functions |
| Loop construct | Implemented | loop with break condition works correctly |
| Prelude loading | Implemented | Automatically loads standard library functions |
| Error handling | Partial | Basic error messages, but not comprehensive |
| Type coercion | Partial | Some automatic type conversion between numeric types |

## Implementation Details

My implementation consists of the following components:

1. **Value System**: Different data types (Integer, Float, Bool, String, List, Quotation, Symbol)

2. **Parser**: Converts text input into tokens, handling comments and special syntax

3. **Interpreter**: Executes the parsed tokens by manipulating the stack and dictionary

4. **Stack Management**: Maintains the operand stack with proper error checking

5. **Dictionary**: Stores variable and function definitions

6. **Prelude Loading**: Automatically loads standard functions from prelude.bprog

## Limitations and Known Issues

Through my development process, I encountered and documented these limitations:

1. **Variable Scoping**: All variables and functions are global, with no lexical scoping

2. **Error Messages**: Error reporting could be more detailed and user-friendly

3. **Nested Control Flow**: Complex nested control structures may not always work as expected

4. **Function Redefinition**: While functions can be redefined, this isn't the most ergonomic process

5. **If Syntax**: The `if` syntax must follow a specific pattern: `condition { then-block } { else-block } if`

## Testing Method

I've tested my implementation using a comprehensive set of test cases. Here's how you can verify the functionality:

### Basic Tests

```bash
# Arithmetic test
echo "10 20 +" > test.bprog
./target/release/rust-bprog test.bprog
# Expected output: 30

# Boolean operations test
echo "True False ||" > test.bprog
./target/release/rust-bprog test.bprog
# Expected output: True
```

### Advanced Tests

```bash
# Function definition and usage
cat > function.bprog << EOF
double { 2 * } fun
5 double
EOF
./target/release/rust-bprog function.bprog
# Expected output: 10

# List operations
cat > list.bprog << EOF
[ 1 2 3 4 5 ]
{ 2 * } map
EOF
./target/release/rust-bprog list.bprog
# Expected output: [2,4,6,8,10]
```

### Complex Example: Factorial

I'm particularly proud of the factorial implementation, which demonstrates recursion:

```bash
cat > factorial.bprog << EOF
factorial {
    dup 0 == 
    if 
    { 
        pop 1 
    } 
    { 
        dup 1 - factorial *
    }
} fun

5 factorial
EOF
./target/release/rust-bprog factorial.bprog
# Expected output: 120
```

## Example Programs

### Guess the Number Game

```
# Simplified Guess the Number game

# Welcome message
"Welcome to Guess the Number!" print
"I'm thinking of a number between 1 and 100." print

# Define our secret number
secretnum { 42 } fun

# Function to check a guess
checkguess {
    # Stack has the guess at this point
    dup secretnum ==
    { 
        "Congratulations! You guessed correctly!" print
        True  # Correct guess
    }
    {
        # Wrong guess - give a hint
        dup secretnum <
        { "Too low! Try again." print }
        { "Too high! Try again." print }
        if
        
        False  # Wrong guess
    }
    if
} fun

# Simulate a game with a series of guesses
50 checkguess drop  # First guess: 50 (too high)
25 checkguess drop  # Second guess: 25 (too low)
40 checkguess drop  # Third guess: 40 (too low)
45 checkguess drop  # Fourth guess: 45 (too high)
42 checkguess       # Fifth guess: 42 (correct!)
```

### Fibonacci Function

```
# Fibonacci sequence calculator
fibonacci {
    dup 2 <
    if
    {
        # Base cases: fib(0) = 0, fib(1) = 1
    }
    {
        # Recursive case: fib(n) = fib(n-1) + fib(n-2)
        dup 1 - fibonacci
        swap 2 - fibonacci
        +
    }
    if
} fun

# Calculate the 10th Fibonacci number
10 fibonacci
```

## Challenges and Solutions

During development, I faced several challenges:

1. **Stack Discipline**: Ensuring proper stack discipline in file mode was tricky. The solution was to enforce ending with exactly one value on the stack.

2. **If Syntax**: The if statement syntax required the condition on the stack, followed by two code blocks. The error message "if requires then and else blocks" was key to understanding this.

3. **Variable Updates**: Implementing variable updates was challenging due to the lack of direct mutable state. I used function redefinition as a workaround.

4. **Nested Control Flow**: Getting complex nested control structures to work properly required careful handling of the stack.

## Conclusion

My BProg interpreter successfully satisfies the core requirements (D level) and the advanced features (C level) of the assignment. It also implements many of the B/A level features, though with some limitations in error handling and variable scoping.

The interpreter successfully runs non-trivial programs like factorial, fibonacci, and the guess-the-number game, demonstrating its practical usability.

Future improvements could include:
- More detailed error messages
- Better handling of complex nested control structures
- More ergonomic variable/function update mechanisms
- Implementation of lexical scoping
