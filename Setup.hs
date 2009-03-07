import Distribution.Simple
import Distribution.Simple.Build (build)
import Distribution.Simple.Setup (defaultBuildFlags)
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..))

import Data.List (sort)
import System.Cmd (system)
import System.FilePath
import System.Directory (getDirectoryContents)

main = defaultMainWithHooks $ simpleUserHooks { runTests = myTestRunner }

testDir :: FilePath
testDir = "tests"

myTestRunner :: Args -> Bool -> PackageDescription -> LocalBuildInfo -> IO ()
myTestRunner _ _ pkg_descr lbi =
    do testInfo <- findAllTest testDir
       let tests = map makeExecutable testInfo
           pkg_descr' = pkg_descr { library = Nothing, executables = tests }
       build pkg_descr' lbi defaultBuildFlags []
       executeAllTests (buildDir lbi) testInfo
    where 
          makeExecutable testInfo =
              Executable (testName testInfo) (testFile testInfo) bif
          bif = maybe emptyBuildInfo libBuildInfo (library pkg_descr)

data TestInfo = TestInfo { testFile :: FilePath
                         , testName :: String }
                deriving Show

findAllTest :: FilePath -> IO [TestInfo]
findAllTest dir = do
  files <- getDirectoryContents dir
  let hss = filter ((== ".hs") . takeExtension) files
  return $ map makeTestInfo hss
    where makeTestInfo hs = TestInfo (dir </> hs) (dropExtension hs)

executeAllTests :: FilePath -> [TestInfo] -> IO ()
executeAllTests buildDir tests = do
  mapM_ exec . sort $ map ((buildDir </>) . dup . testName) tests
    where dup path = path </> path
          exec path = putStrLn ("* " ++ path) >> system path
