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

incomingCalls :: HieDb -> Symbol -> IO [Vertex]
incomingCalls (getConn -> conn) symbol = do
  let (o, m, u) = parseSymbol symbol
  query conn "select distinct mods.mod, defs.occ, mods.hs_src, defs.sl, defs.sc, \
              \defs.el, defs.ec, refs.sl, refs.sc, refs.el, refs.ec \
              \from refs \
              \join decls on decls.hieFile = refs.hieFile \
              \join defs on defs.hieFile = decls.hieFile and defs.occ = decls.occ \
              \join mods on mods.hieFile = decls.hieFile \
              \where \
              \(refs.occ = ? and refs.mod = ? and refs.unit = ?) \
              \and \
              \(decls.occ != ? or mods.mod != ? or mods.unit != ?) \
              \and \
              \((refs.sl = decls.sl and refs.sc > decls.sc) or (refs.sl > decls.sl)) \
              \and \
              \((refs.el = decls.el and refs.ec <= decls.ec) or (refs.el < decls.el))" (o, m, u, o, m, u)

outgoingCalls :: HieDb -> Symbol -> IO [Vertex]
outgoingCalls (getConn -> conn) Symbol{..} = do
  let n = toNsChar (occNameSpace symName) : occNameString symName
      m = moduleNameString $ moduleName symModule
      u = unitIdString $ moduleUnitId symModule
  query conn "select distinct rm.mod, defs.occ, rm.hs_src, defs.sl, defs.sc, defs.el, defs.ec, \
            \refs.sl, refs.sc, refs.el, refs.ec \
            \from refs \
            \join defs on defs.occ = refs.occ \
            \join decls rd on rd.hieFile = defs.hieFile and rd.occ = defs.occ \
            \join mods rm on rm.mod = refs.mod and rm.unit = refs.unit and rm.hieFile = defs.hieFile \
            \join decls on decls.hieFile = refs.hieFile \
            \join mods on mods.hieFile = decls.hieFile \
            \where \
            \(decls.occ = ? and mods.mod = ? and mods.unit = ?)  \
            \and \
            \(defs.occ != ? or rm.mod != ? or rm.unit != ?) \
            \and \
            \((refs.sl > decls.sl) OR (refs.sl = decls.sl AND refs.sc >  decls.sc)) \
            \and \
            \((refs.el < decls.el) OR (refs.el = decls.el AND refs.ec <= decls.ec))" (n, m, u, n, m, u)

parseSymbol :: Symbol -> (String, String, String)
parseSymbol Symbol{..} =
  let o = toNsChar (occNameSpace symName) : occNameString symName
      m = moduleNameString $ moduleName symModule
      u = unitIdString $ moduleUnitId symModule
  in  (o, m, u)
