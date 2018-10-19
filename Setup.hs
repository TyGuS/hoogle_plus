import Distribution.Simple
import Language.Java.Inline.Cabal (gradleHooks)
import System.Directory

main = do
    currDir <- getCurrentDirectory
    let buildFile = "apply plugin: 'java'\n"
                 ++ "apply plugin: 'application'\n"
                 ++ "\n"
                 ++ "mainClassName = \"bogus.class.name\"\n"
                 ++ "\n"
                 ++ "dependencies {\n"
                 ++ "    compile files('" ++ currDir ++ "/src/sypet/sypet.jar'\n"
                 ++ "                 ,'" ++ currDir ++ "/src/sypet/lib/apt.jar'\n"
                 ++ "                 ,'" ++ currDir ++ "/src/sypet/lib/commons-lang3-3.4.jar')\n"
                 ++ "}"
    writeFile "build.gradle" buildFile
    defaultMainWithHooks (gradleHooks simpleUserHooks)