@echo off

rem Create a cabal.project.local file containing the path to the local SDL2 libraries

(
echo.package sdl2
echo.    extra-lib-dirs: "%cd:\=\\%\\sdl2_win_mingw\\lib"
echo.    extra-include-dirs: "%cd:\=\\%\\sdl2_win_mingw\\include\\SDL2"
)>"cabal.project.local"
echo [32mSetup complete! Try running '[107m[30mcabal run[0m[32m' to build the project.[0m
echo If you encounter problems, try asking Discord.
