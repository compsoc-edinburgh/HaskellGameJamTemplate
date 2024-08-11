@echo off

rem Don't run if the sdl2_local_win folder or cabal.project.local already exists
if exist sdl2_local_win (
    echo [31mError: The 'sdl2_local_win' folder already exists.[0m
    echo If you want to re-run the setup, delete the 'sdl2_local_win' folder and 'cabal.project.local' file and try again.
    exit /b
)
if exist cabal.project.local (
    echo [31mError: The 'cabal.project.local' file already exists.[0m
    echo If you want to re-run the setup, delete the 'sdl2_local_win' folder and 'cabal.project.local' file and try again.
    exit /b
)

rem Download the SDL2 2.30.6 libraries
rem Use powershell, as curl may not be installed
echo.[32minfo [0m Downloading SDL 2.30.6 libraries...
powershell -Command "& {$ProgressPreference='SilentlyContinue'; Invoke-WebRequest https://github.com/libsdl-org/SDL/releases/download/release-2.30.6/SDL2-devel-2.30.6-mingw.zip -OutFile SDL2-devel-2.30.6-mingw.zip}"

rem Extract the SDL2 libraries
rem Use powershell, as tar may not be installed
echo.[32minfo [0m Extracting SDL 2.30.6 libraries...
powershell -Command "& {Add-Type -AssemblyName System.IO.Compression.FileSystem; [System.IO.Compression.ZipFile]::ExtractToDirectory('SDL2-devel-2.30.6-mingw.zip', 'SDL2-devel-2.30.6-mingw')}"

rem Remove the zip file
echo.[32minfo [0m Removing downloaded SDL2-devel-2.30.6-mingw.zip file...
del SDL2-devel-2.30.6-mingw.zip

rem Copy the bin, include and lib folders to the sdl2_local_win folder
echo.[32minfo [0m Making new sdl2_local_win folder...
mkdir sdl2_local_win
echo.[32minfo [0m Copying SDL2 libraries to new sdl2_local_win folder...
xcopy /E /Y SDL2-devel-2.30.6-mingw\SDL2-2.30.6\x86_64-w64-mingw32\bin sdl2_local_win\bin\ > nul
xcopy /E /Y SDL2-devel-2.30.6-mingw\SDL2-2.30.6\x86_64-w64-mingw32\include sdl2_local_win\include\ > nul
xcopy /E /Y SDL2-devel-2.30.6-mingw\SDL2-2.30.6\x86_64-w64-mingw32\lib sdl2_local_win\lib\ > nul

rem Remove the extracted SDL2 folder
echo.[32minfo [0m Removing extracted SDL2-devel-2.30.6-mingw folder...
rmdir /S /Q SDL2-devel-2.30.6-mingw

rem Create a cabal.project.local file containing the path to the local SDL2 libraries
echo.[32minfo [0m Creating cabal.project.local file...
(
echo.package sdl2
echo.    -- Disable pkg-config, as we instead supply the path to the SDL2 libraries manually
echo.    flags: -pkgconfig
echo.    -- Supply the path to the local SDL2 libraries (must be absolute path^)
echo.    extra-lib-dirs: "%cd:\=\\%\\sdl2_local_win\\lib"
echo.    extra-include-dirs: "%cd:\=\\%\\sdl2_local_win\\include\\SDL2"
echo.package text
echo.    -- Disable use of SIMD, which makes text fails to build on Win + GHC 8.8.4
echo.    flags: -simdutf
)>"cabal.project.local"
echo [32mSetup complete! Try running '[107m[30mcabal run[0m[32m' to build the project.[0m
echo If you encounter problems, try asking on Discord.
