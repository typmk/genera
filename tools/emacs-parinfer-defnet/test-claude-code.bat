@echo off
REM Test script for claude-code.el integration
REM Run this after starting Emacs GUI

setlocal enabledelayedexpansion

set "EMACSCLIENT=C:\Program Files\Emacs\emacs-30.2\bin\emacsclient.exe"

echo ===================================
echo Claude Code Emacs Integration Test
echo ===================================
echo.

REM Check if emacsclient exists
if not exist "%EMACSCLIENT%" (
    echo ERROR: emacsclient.exe not found at:
    echo   %EMACSCLIENT%
    exit /b 1
)

REM Test 1: Check if server is running
echo Checking if Emacs server is running...
"%EMACSCLIENT%" --eval "(server-running-p)" 2>nul | find "t" >nul
if errorlevel 1 (
    echo FAILED: Emacs server not running
    echo.
    echo Please start Emacs GUI first.
    echo The server will start automatically from your init.el
    exit /b 1
)
echo OK: Server is running
echo.

REM Test 2: Check if claude-code is loaded
echo Test 1: Checking if claude-code is loaded...
"%EMACSCLIENT%" --eval "(featurep 'claude-code)" 2>nul | find "t" >nul
if errorlevel 1 (
    echo   FAILED
) else (
    echo   OK
)

REM Test 3: Check program name
echo Test 2: Checking claude-code-program setting...
"%EMACSCLIENT%" --eval "claude-code-program"
echo.

REM Test 4: Check if mode is enabled
echo Test 3: Checking if claude-code-mode is active...
"%EMACSCLIENT%" --eval "claude-code-mode" 2>nul | find "t" >nul
if errorlevel 1 (
    echo   FAILED or not active
) else (
    echo   OK
)

REM Test 5: Check if claude command exists
echo Test 4: Checking if 'claude' command is accessible...
where claude >nul 2>&1
if errorlevel 1 (
    echo   FAILED: claude command not in PATH
) else (
    claude --version
    echo   OK
)

echo.
echo ===================================
echo.
echo If all tests passed, you can use C-c c in Emacs to access Claude Code.
echo.
echo Quick test in Emacs:
echo   1. Press C-c c c to start Claude
echo   2. Or press C-c c m to see the menu
echo.

pause
