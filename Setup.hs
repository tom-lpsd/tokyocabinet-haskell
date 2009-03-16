import Distribution.Simple
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..))

import System.Cmd (system)
import System.FilePath

main = defaultMainWithHooks $ simpleUserHooks { runTests = myTestRunner }

myTestRunner :: Args -> Bool -> PackageDescription -> LocalBuildInfo -> IO ()
myTestRunner _ _ pkg_descr lbi =  mapM_ (system . path) $ executables pkg_descr
    where
      path exec = let name = (dropExtension . exeName) exec
                  in (buildDir lbi) </> name </> name
