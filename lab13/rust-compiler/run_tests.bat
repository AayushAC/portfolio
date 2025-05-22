@echo off
REM Simple BProg compiler test script for Windows

echo === BPROG COMPILER TEST SCRIPT ===
echo.

echo Testing arithmetic operations...
echo 10 20 + > test_input.bprog
cargo run test_input.bprog
echo.

echo Testing function definition...
echo double { 2 * } fun > test_input.bprog
echo 5 double >> test_input.bprog
cargo run test_input.bprog
echo.

echo Testing stack operations...
echo 5 dup + > test_input.bprog
cargo run test_input.bprog
echo.

echo Testing boolean operations...
echo True False ^|^| > test_input.bprog
cargo run test_input.bprog
echo.

echo Testing comprehensive example...
echo # Comprehensive test > test_input.bprog
echo double { 2 * } fun >> test_input.bprog
echo triple { 3 * } fun >> test_input.bprog
echo 7 double triple + >> test_input.bprog
cargo run test_input.bprog
echo.

echo Testing REPL mode...
echo.
echo Enter these expressions manually to test REPL:
echo 1. 10 20 +
echo 2. double { 2 * } fun 5 double
echo 3. True False ^|^|
echo 4. Type 'exit' to quit
echo.

echo === TEST SUMMARY ===
echo The compiler has generated WAT files for each test case.
echo Verify that each WAT file contains proper WebAssembly code
echo with the expected operations.
echo.

pause