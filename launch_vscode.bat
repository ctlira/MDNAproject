@echo off
REM --- Load Intel oneAPI environment ---
set VS2022INSTALLDIR=C:\Program Files\Microsoft Visual Studio\2022\Community
call "C:\Program Files (x86)\Intel\oneAPI\setvars.bat" intel64 vs2022

REM --- Change directory to the folder containing this .bat file ---
cd /d "%~dp0"

REM --- Launch VS Code in this folder ---
code .
