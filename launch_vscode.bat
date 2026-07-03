@echo off
REM --- Load Intel oneAPI environment ---
call "C:\Program Files (x86)\Intel\oneAPI\setvars.bat"

REM --- Change directory to the folder containing this .bat file ---
cd /d "%~dp0"

REM --- Launch VS Code in this folder ---
code .
