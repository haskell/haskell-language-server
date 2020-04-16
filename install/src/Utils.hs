module Utils where 

import System.Info.Extra

-- | The extension of executables, @\"exe\"@ on Windows and @\"\"@ otherwise.
exe :: String
exe = if isWindows then "exe" else ""
