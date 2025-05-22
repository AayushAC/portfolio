@echo off
REM WAT file verification helper script

echo === WAT FILE VERIFICATION HELPER ===
echo This script will display the generated WAT files for verification
echo.

IF NOT EXIST test_input.wat (
    echo No test_input.wat file found! Run the tests first.
    goto end
)

echo Showing WAT file content:
echo ===========================
type test_input.wat
echo ===========================

echo.
echo Check that the WAT code:
echo 1. Has proper module structure
echo 2. Correctly translates BProg operations to WAT instructions
echo 3. Defines functions properly (if applicable)
echo 4. Handles stack operations correctly
echo.

:end
pause