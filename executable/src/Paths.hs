module Paths (dataDir
             ,replHistory
             ,importDirs) where

import Lusp.Evaluate (stdlibInstallDir)

import System.FilePath ((</>)
                        ,addTrailingPathSeparator)
import System.Directory (getAppUserDataDirectory)

dataDir :: IO FilePath
dataDir = getAppUserDataDirectory "lusp"

replHistory :: IO FilePath
replHistory = (</> "history") <$> dataDir

importDirs :: IO [FilePath]
importDirs = (: ["/usr/local/lib/lusp/"
                ,"/usr/lib/lusp/"])
    <$> addTrailingPathSeparator <$> stdlibInstallDir
