(module
  ;; Import memory for string and list operations
  (memory (export "memory") 1)

  ;; Global stack pointer
  (global $sp (mut i32) (i32.const 0))

  ;; Import JavaScript functions
  (import "console" "log" (func $log (param i32)))
  (import "env" "read_line" (func $read_line (result i32)))

  (func $double (result i32)
    ;; Local variables for stack operations
    (local $temp i32)
    (local $temp_a i32)
    (local $temp_b i32)

    i32.const 2
    i32.mul
    ;; Ensure there's a return value (default to 0 if stack is empty)
    (if (result i32)
      (i32.eq (global.get $sp) (i32.const 0))
      (then (i32.const 0))
      (else
        ;; Return the top value from the stack
        ;; In a real implementation, we would pop it from a proper stack
        ;; For now, we'll just return whatever is on top of the WebAssembly stack
      )
    )
  )

  (func $triple (result i32)
    ;; Local variables for stack operations
    (local $temp i32)
    (local $temp_a i32)
    (local $temp_b i32)

    i32.const 3
    i32.mul
    ;; Ensure there's a return value (default to 0 if stack is empty)
    (if (result i32)
      (i32.eq (global.get $sp) (i32.const 0))
      (then (i32.const 0))
      (else
        ;; Return the top value from the stack
        ;; In a real implementation, we would pop it from a proper stack
        ;; For now, we'll just return whatever is on top of the WebAssembly stack
      )
    )
  )

  (func $main (export "main") (result i32)
    ;; Local variables for stack operations
    (local $temp i32)
    (local $temp_a i32)
    (local $temp_b i32)

    call $double
    ;; Begin quotation
    i32.const 2
    i32.mul
    ;; Ensure there's a return value (default to 0 if stack is empty)
    (if (result i32)
      (i32.eq (global.get $sp) (i32.const 0))
      (then (i32.const 0))
      (else
        ;; Return the top value from the stack
        ;; In a real implementation, we would pop it from a proper stack
        ;; For now, we'll just return whatever is on top of the WebAssembly stack
      )
    )
    ;; End quotation
    ;; Function definition (handled at extraction phase)
    call $triple
    ;; Begin quotation
    i32.const 3
    i32.mul
    ;; Ensure there's a return value (default to 0 if stack is empty)
    (if (result i32)
      (i32.eq (global.get $sp) (i32.const 0))
      (then (i32.const 0))
      (else
        ;; Return the top value from the stack
        ;; In a real implementation, we would pop it from a proper stack
        ;; For now, we'll just return whatever is on top of the WebAssembly stack
      )
    )
    ;; End quotation
    ;; Function definition (handled at extraction phase)
    i32.const 7
    call $double
    call $triple
    i32.add
    ;; Ensure there's a return value (default to 0 if stack is empty)
    (if (result i32)
      (i32.eq (global.get $sp) (i32.const 0))
      (then (i32.const 0))
      (else
        ;; Return the top value from the stack
        ;; In a real implementation, we would pop it from a proper stack
        ;; For now, we'll just return whatever is on top of the WebAssembly stack
      )
    )
  )
)
