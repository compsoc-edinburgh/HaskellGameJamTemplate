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
import Data.Maybe
    ( fromJust
    )
import System.Directory
    ( getCurrentDirectory
    )

main :: IO ()
main = defaultMainWithHooks simpleUserHooks { confHook = sdlConfHook }

sdlConfHook :: (GenericPackageDescription, HookedBuildInfo) -> ConfigFlags -> IO LocalBuildInfo
sdlConfHook (description, buildInfo) flags = do
    localBuildInfo <- confHook simpleUserHooks (description, buildInfo) flags
    let packageDescription = localPkgDescr localBuildInfo
        library = fromJust $ library packageDescription
        libraryBuildInfo = libBuildInfo library
    dir <- getCurrentDirectory
    return localBuildInfo {
        localPkgDescr = packageDescription {
            library = Just $ library {
                libBuildInfo = libraryBuildInfo {
                    includeDirs = (dir ++ "/sdl/include"):(includeDirs libraryBuildInfo),
                    extraLibDirs = (dir ++ "/sdl/lib"):(extraLibDirs libraryBuildInfo)
                }
            }
        }
    }
