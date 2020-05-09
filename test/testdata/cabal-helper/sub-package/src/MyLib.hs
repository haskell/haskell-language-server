module MyLib (someFunc) where

import qualified PluginLib as L

someFunc :: IO ()
someFunc = L.someFunc
