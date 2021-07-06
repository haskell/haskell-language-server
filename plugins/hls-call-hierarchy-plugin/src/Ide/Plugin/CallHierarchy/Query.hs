{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}
module Ide.Plugin.CallHierarchy.Query where

import           Database.SQLite.Simple
import           GHC
import           HieDb                          (HieDb (getConn), Symbol (..),
                                                 toNsChar)
import qualified HieDb
import           Ide.Plugin.CallHierarchy.Types
import           Module
import           Name

getReachableFrom :: HieDb -> Vertex -> IO [Vertex]
getReachableFrom (getConn -> conn) v = undefined

incomingCalls :: HieDb -> Symbol -> IO [Vertex]
incomingCalls (getConn -> conn) Symbol{..} = do
  let n = toNsChar (occNameSpace symName) : occNameString symName
      m = moduleNameString $ moduleName symModule
      u = unitIdString $ moduleUnitId symModule
  query conn "select distinct rm.mod, defs.occ, rm.hs_src, defs.sl, defs.sc, defs.el, defs.ec \
            \refs.sl, refs.sc, refs.el, refs.ec \
            \from refs \
            \join defs on defs.occ = refs.occ \
            \join decls rd on rd.hieFile = defs.hieFile and rd.occ = defs.occ \
            \join mods rm on rm.mod = refs.mod and rm.unit = refs.unit and rm.hieFile = defs.hieFile \
            \join decls on decls.hieFile = refs.hieFile \
            \join mods on mods.hieFile = decls.hieFile \
            \where \
            \(decls.occ = ? and defs.occ != ? and mods.mod = ? and mods.unit = ?) \
            \and \
            \((refs.sl > decls.sl) OR (refs.sl = decls.sl AND refs.sc >  decls.sc)) \
            \and \
            \((refs.el < decls.el) OR (refs.el = decls.el AND refs.ec <= decls.ec))" (n, n, m, u)

outgoingCalls :: HieDb -> Symbol -> IO [Vertex]
outgoingCalls = incomingCalls
