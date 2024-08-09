import Distribution.PackageDescription
    ( GenericPackageDescription
    , HookedBuildInfo
    , extraLibDirs
    , includeDirs
    , libBuildInfo
    , library
    )
import Distribution.Simple
    ( Args
    , UserHooks
    , confHook
    , defaultMainWithHooks
    , simpleUserHooks
    )
import Distribution.Simple.LocalBuildInfo
    ( LocalBuildInfo
    , localPkgDescr
    )
import Distribution.Simple.Setup
    ( ConfigFlags
    )
import Distribution.System
    ( buildOS
    , OS (Windows)
    )
import Data.Maybe
    ( fromJust
    )
import System.Directory
    ( getCurrentDirectory
    )

main :: IO ()
main = defaultMainWithHooks simpleUserHooks { confHook = sdlConfHook }

-- | A hook that adds the SDL include and lib directories to the build info on
-- Windows. This is necessary to link against the SDL library during compilation.
--
-- On MacOS and Linux, the SDL library can be installed using package managers,
-- which puts them in a standard system-wide location that GHC will find
-- automatically. However, on Windows, due to Haskell being distributed not
-- natively but on top of MinGW and MSYS2, installing and using external
-- libraries is a much more manual and error-prone process.
--
-- Instead, in this Game Jam Template repository, we've bundled the Windows
-- build of the SDL development libraries with the project, so that we can avoid
-- a system-wide installation of SDL. But because it's now in a non-standard
-- location, we need to tell GHC where to find those libraries. We could specify
-- the path in the .cabal or cabal.project files, but they only accept absolute
-- paths and not relative ones. Which means it's unsuited for a template repo
-- that might be built on many different machines. Thus, we chose to use a
-- custom Setup.hs file, which runs during the build process and basically
-- dynamically modifies the .cabal file to add absolute paths before GHC reads it.
sdlConfHook :: (GenericPackageDescription, HookedBuildInfo) -> ConfigFlags -> IO LocalBuildInfo
sdlConfHook (description, buildInfo) flags =
    -- Only add the SDL include and lib directories on Windows
    if buildOS /= Windows
        then pure $ fromJust $ confHook simpleUserHooks (description, buildInfo) flags
        else do
            localBuildInfo <- confHook simpleUserHooks (description, buildInfo) flags
            let packageDescription = localPkgDescr localBuildInfo
                library = fromJust $ library packageDescription
                libraryBuildInfo = libBuildInfo library
            dir <- getCurrentDirectory
            pure localBuildInfo {
                localPkgDescr = packageDescription {
                    library = Just $ library {
                        libBuildInfo = libraryBuildInfo {
                            includeDirs = (dir ++ "/sdl2_win_mingw/include"):(includeDirs libraryBuildInfo),
                            extraLibDirs = (dir ++ "/sdl2_win_mingw/lib"):(extraLibDirs libraryBuildInfo)
                        }
                    }
                }
            }
