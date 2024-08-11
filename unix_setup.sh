#!/bin/sh

# Don't run if the sdl2_local folder or cabal.project.local already exists
if [ -d "sdl2_local" ] || [ -f "cabal.project.local" ]; then
    echo "sdl2_local folder or cabal.project.local already exists."
    echo "Please remove them before running this script."
    exit 1
fi

# Download the SDL2 2.30.6 libraries
printf "\033[32minfo \033[0m Downloading SDL 2.30.6 libraries...\n"
curl -OLsS https://github.com/libsdl-org/SDL/releases/download/release-2.30.6/SDL2-2.30.6.tar.gz

# Extract the SDL2 2.30.6 libraries
printf "\033[32minfo \033[0m Extracting SDL 2.30.6 libraries...\n"
tar -xzf SDL2-2.30.6.tar.gz

# Remove the SDL2 2.30.6 tarball
printf "\033[32minfo \033[0m Removing downloaded SDL2-2.30.6.tar.gz file...\n"
rm SDL2-2.30.6.tar.gz

# Create a build directory
printf "\033[32minfo \033[0m Temporarily changing dir to SDL source dir...\n"
cd SDL2-2.30.6 || (printf "\033[31mError: SDL2-2.30.6 folder not found.\033[0m\n" && exit 1)
mkdir build
cd build || (printf "\033[31mError: SDL2-2.30.6/build folder not found.\033[0m\n" && exit 1)

printf "\033[32minfo \033[0m Configuring SDL2 to prepare build...\n"
if ! output=$(../configure --prefix="$(cd ../../ && pwd)/sdl2_local" 2>&1); then
    printf "\033[31mError configuring SDL2. Check sdl_build_log.txt for more information.\033[0m\n"
    echo "$output" >> sdl_build_log.txt
    exit 1
fi
printf "\033[32minfo \033[0m Building SDL2 libraries...\n"
if ! output=$(make 2>&1); then
    printf "\033[31mError building SDL2. Check sdl_build_log.txt for more information.\033[0m\n"
    echo "$output" >> sdl_build_log.txt
    exit 1
fi
printf "\033[32minfo \033[0m Copying built libraries to new sdl2_local folder...\n"
if ! output=$(make install 2>&1); then
    printf "\033[31mError copying SDL2. Check sdl_build_log.txt for more information.\033[0m\n"
    echo "$output" >> sdl_build_log.txt
    exit 1
fi
# Remove log file if it exists because it was successful
rm sdl_build_log.txt 2>/dev/null

printf "\033[32minfo \033[0m Removing extracted SDL2-2.30.6 directory...\n"
cd ../../
rm -r SDL2-2.30.6

# Create a cabal.project.local file
printf "\033[32minfo \033[0m Creating cabal.project.local file...\n"
cat <<EOF >> cabal.project.local
package sdl2
    flags: -pkgconfig
    extra-lib-dirs: $(pwd)/sdl2_local/lib
    extra-include-dirs: $(pwd)/sdl2_local/include/SDL2
EOF

printf "\033[32mSetup complete! Try running '\033[107m\033[30mcabal run\033[0m\033[32m' to build the project.\033[0m\n"
printf "If you encounter problems, try asking on Discord.\n"


