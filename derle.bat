@echo off
setlocal
rem ---------------------------------------------------------------
rem  Gokyuzu Adalari - derleme betigi
rem  cabal build yapar, exe'yi proje kokune ve DLL'i exe yanina kopyalar
rem ---------------------------------------------------------------
cd /d "%~dp0"

where cabal >nul 2>&1
if errorlevel 1 (
  echo [HATA] cabal bulunamadi.
  echo        GHC + cabal icin: https://www.haskell.org/ghcup/
  pause
  exit /b 1
)

echo [1/3] Derleniyor...
cabal build
if errorlevel 1 (
  echo.
  echo [HATA] Derleme basarisiz oldu.
  pause
  exit /b 1
)

echo [2/3] Calistirilabilir dosya kopyalaniyor...
for /f "delims=" %%i in ('cabal list-bin platformer') do set "EXE=%%i"
for %%F in ("%EXE%") do set "EXEDIR=%%~dpF"
copy /y "%EXE%" ".\platformer.exe" >nul

echo [3/3] freeglut.dll kontrol ediliyor...
if exist "freeglut.dll" (
  copy /y "freeglut.dll" "%EXEDIR%" >nul
  echo       freeglut.dll hazir.
) else (
  echo       [UYARI] freeglut.dll bulunamadi.
  echo       Indir: https://github.com/freeglut/freeglut/releases
  echo       Dosyayi bu klasore koyup derle.bat'i tekrar calistirin.
)

echo.
echo Tamamlandi. Oyunu baslatmak icin: oyna.bat
pause
