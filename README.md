# Haskell Game Jam Template

This is a template repository for a game written in Haskell!

Comments are sprinkled wherever possible to make it easier to understand.

The "core" (where the update/draw loop is) is in `Game.hs`, check out the `updateState` and `drawState` functions!

## Using this template

This project has been tested on GHC 8.8.4, 8.10.7, and 9.4.8, on Windows 11, macOS Sonoma (Arm), and DICE Ubuntu.

This project uses SDL2 as the cross-platform window/graphics abstraction library.
So to build this project correctly, we'll need to install SDL2 so Haskell can find it.
You can do this either system-wide or local to this project.

- System-wide SDL2 installation (recommended if supported)
  - On DICE:
    - This is unsupported.
  - On Windows:
    - This is unsupported.
  - On Linux (Ubuntu):
    - Run `apt-get install libsdl2-dev`
  - On Linux (other):
    - Find the SDL2 development package from your system package manager.
  - On MacOS:
    - Run `brew install sdl2` assuming you have Homebrew. If you do not, follow the local installation.
- Local SDL2 installation
  - On DICE/MacOS/Ubuntu/other:
    - Run `./unix_setup.sh`, which relies only on `tar`, `curl`, `make` and standard C compilation tools.
  - On Windows:
    - Run `.\win_setup.bat`.

After SDL2 is setup, run the following to build the project and run it:

```
cabal run
```

### Understanding

The project template starts a fixed-FPS game loop (at 60FPS), which handles the movement of a 2D sprite.

Feel free to expand this in any direction you want! (Even 3D is possible if you wrangle with it enough)

### Adding dependencies

To add dependencies to the project, edit the `<project-name>.cabal` file's `build-depends` clause.

### Packaging

Packaging the game can be a little complex if you're on Linux or Mac, since the game binary might contain an absolute path reference to the SDL libraries, which will be broken if you move it to another machine. Please investigate "SDL game distribution" on your own :(

For Windows:
- Run `cabal list-bin Game` to find where the built binary is
- Copy the SDL2.dll file, your game binary, and whatever other assets/resources you need, into a new folder
- Test by double-clicking the game binary to see that it runs.
- ZIP that folder up for distribution.

### Stack

This project does not support using Stack. You can try, but if you already have Cabal installed, it's much quicker to use that.
