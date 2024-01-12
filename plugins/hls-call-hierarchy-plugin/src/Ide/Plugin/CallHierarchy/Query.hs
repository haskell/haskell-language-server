{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE ViewPatterns     #-}

module Ide.Plugin.CallHierarchy.Query (
  incomingCalls
, outgoingCalls
, getSymbolPosition
) where

import qualified Data.Text                      as T
import           Database.SQLite.Simple
import           Development.IDE.GHC.Compat
import           HieDb                          (HieDb (getConn), Symbol (..))
import           Ide.Plugin.CallHierarchy.Types

incomingCalls :: HieDb -> Symbol -> IO [Vertex]
incomingCalls (getConn -> conn) symbol = do
    let (o, m, u) = parseSymbol symbol
    query conn
        (Query $ T.pack $ concat
            [ "SELECT mods.mod, decls.occ, mods.hs_src, decls.sl, decls.sc, "
            , "decls.el, decls.ec, refs.sl, refs.sc, refs.el, refs.ec "
            , "FROM refs "
            , "JOIN decls ON decls.hieFile = refs.hieFile "
            , "JOIN mods ON mods.hieFile = decls.hieFile "
            , "where "
            , "(refs.occ = ? AND refs.mod = ? AND refs.unit = ?) "
            , "AND "
            , "(decls.occ != ? OR mods.mod != ? OR mods.unit != ?) "
            , "AND "
            , "((refs.sl = decls.sl AND refs.sc > decls.sc) OR (refs.sl > decls.sl)) "
            , "AND "
            ,"((refs.el = decls.el AND refs.ec <= decls.ec) OR (refs.el < decls.el))"
            ]
        ) (o, m, u, o, m, u)

outgoingCalls :: HieDb -> Symbol -> IO [Vertex]
outgoingCalls (getConn -> conn) symbol = do
    let (o, m, u) = parseSymbol symbol
    query conn
        (Query $ T.pack $ concat
            [ "SELECT rm.mod, defs.occ, rm.hs_src, defs.sl, defs.sc, defs.el, defs.ec, "
            , "refs.sl, refs.sc, refs.el, refs.ec "
            , "from refs "
            , "JOIN defs ON defs.occ = refs.occ "
            , "JOIN decls rd ON rd.hieFile = defs.hieFile AND rd.occ = defs.occ "
            , "JOIN mods rm ON rm.mod = refs.mod AND rm.unit = refs.unit AND rm.hieFile = defs.hieFile "
            , "JOIN decls ON decls.hieFile = refs.hieFile "
            , "JOIN mods ON mods.hieFile = decls.hieFile "
            , "where "
            , "(decls.occ = ? AND mods.mod = ? AND mods.unit = ?) "
            , "AND "
            , "(defs.occ != ? OR rm.mod != ? OR rm.unit != ?) "
            , "AND "
            , "((refs.sl = decls.sl AND refs.sc >  decls.sc) OR (refs.sl > decls.sl)) "
            , "AND "
            , "((refs.el = decls.el AND refs.ec <= decls.ec) OR (refs.el < decls.el))"
            ]
        ) (o, m, u, o, m, u)

getSymbolPosition :: HieDb -> Vertex -> IO [SymbolPosition]
getSymbolPosition (getConn -> conn) Vertex{..} = do
    query conn
        (Query $ T.pack $ concat
            [ "SELECT refs.sl, refs.sc from refs where "
            , "(occ = ?) "
            , "AND "
            , "((refs.sl = ? AND refs.sc > ?) OR (refs.sl > ?)) "
            , "AND "
            , "((refs.el = ? AND refs.ec <= ?) OR (refs.el < ?))"
            ]
        ) (occ, sl, sc, sl, el, ec, el)

parseSymbol :: Symbol -> (OccName, ModuleName, Unit)
parseSymbol Symbol{..} =
    let o = symName
        m = moduleName symModule
        u = moduleUnit symModule
    in  (o, m, u)
