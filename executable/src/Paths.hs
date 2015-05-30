module Paths (dataDir
             ,replHistory
             ,importDirs) where

import System.FilePath ((</>))
import System.Directory (getAppUserDataDirectory)

dataDir :: IO FilePath
dataDir = getAppUserDataDirectory "lusp"

replHistory :: IO FilePath
replHistory = (</> "history") <$> dataDir

importDirs :: [FilePath]
importDirs = ["/usr/local/lib/lusp/"
             ,"/usr/lib/lusp/"]
