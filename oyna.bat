@echo off
setlocal
rem ---------------------------------------------------------------
rem  Gokyuzu Adalari - oyunu baslatir
rem ---------------------------------------------------------------
cd /d "%~dp0"

if not exist "platformer.exe" (
  echo platformer.exe bulunamadi.
  echo Once derle.bat calistirin.
  pause
  exit /b 1
)

if not exist "freeglut.dll" (
  echo [UYARI] freeglut.dll bulunamadi, oyun acilmayabilir.
  echo         Indir: https://github.com/freeglut/freeglut/releases
  echo.
)

start "" "platformer.exe"
