{-# LANGUAGE ScopedTypeVariables #-}

module Ide.Plugin.Tactic.Debug
  ( unsafeRender
  , unsafeRender'
  , showAstData
  , BlankSrcSpan (..)
  , traceM
  , traceShowId
  , trace
  ) where

import Prelude hiding ((<>))
import Debug.Trace
import DynFlags (unsafeGlobalDynFlags)

import Data.Data hiding (Fixity)
import Bag
import BasicTypes
import FastString
import NameSet
import Name
import DataCon
import SrcLoc
import HsSyn
import OccName hiding (occName)
import Var
import Module
import Outputable
import qualified Data.ByteString as B
import Generics.SYB hiding (Fixity, empty)
import Id

------------------------------------------------------------------------------
-- | Print something
unsafeRender :: Outputable a => a -> String
unsafeRender = unsafeRender' . ppr

unsafeRender' :: SDoc -> String
unsafeRender' = showSDoc unsafeGlobalDynFlags




data BlankSrcSpan = BlankSrcSpan | NoBlankSrcSpan
                  deriving (Eq,Show)

-- | Show a GHC syntax tree. This parameterised because it is also used for
-- comparing ASTs in ppr roundtripping tests, where the SrcSpan's are blanked
-- out, to avoid comparing locations, only structure
--
-- ripped out of GHC so that I could make it print the types of ids
showAstData :: Data a => BlankSrcSpan -> a -> SDoc
showAstData b a0 = blankLine $$ showAstData' a0
  where
    showAstData' :: Data a => a -> SDoc
    showAstData' =
      generic
              `ext1Q` list
              `extQ` string `extQ` fastString `extQ` srcSpan
              `extQ` lit `extQ` litr `extQ` litt
              `extQ` bytestring
              `extQ` name `extQ` occName `extQ` moduleName `extQ` var
              `extQ` dataCon
              `extQ` bagName `extQ` bagRdrName `extQ` bagVar `extQ` nameSet
              `extQ` fixity
              `ext2Q` located

      where generic :: Data a => a -> SDoc
            generic t = parens $ text (showConstr (toConstr t))
                                  $$ vcat (gmapQ showAstData' t)

            string :: String -> SDoc
            string     = text . normalize_newlines . show

            fastString :: FastString -> SDoc
            fastString s = braces $
                            text "FastString: "
                         <> text (normalize_newlines . show $ s)

            bytestring :: B.ByteString -> SDoc
            bytestring = text . normalize_newlines . show

            list []    = brackets empty
            list [x]   = brackets (showAstData' x)
            list (x1 : x2 : xs) =  (text "[" <> showAstData' x1)
                                $$ go x2 xs
              where
                go y [] = text "," <> showAstData' y <> text "]"
                go y1 (y2 : ys) = (text "," <> showAstData' y1) $$ go y2 ys

            -- Eliminate word-size dependence
            lit :: HsLit GhcPs -> SDoc
            lit (HsWordPrim   s x) = numericLit "HsWord{64}Prim" x s
            lit (HsWord64Prim s x) = numericLit "HsWord{64}Prim" x s
            lit (HsIntPrim    s x) = numericLit "HsInt{64}Prim"  x s
            lit (HsInt64Prim  s x) = numericLit "HsInt{64}Prim"  x s
            lit l                  = generic l

            litr :: HsLit GhcRn -> SDoc
            litr (HsWordPrim   s x) = numericLit "HsWord{64}Prim" x s
            litr (HsWord64Prim s x) = numericLit "HsWord{64}Prim" x s
            litr (HsIntPrim    s x) = numericLit "HsInt{64}Prim"  x s
            litr (HsInt64Prim  s x) = numericLit "HsInt{64}Prim"  x s
            litr l                  = generic l

            litt :: HsLit GhcTc -> SDoc
            litt (HsWordPrim   s x) = numericLit "HsWord{64}Prim" x s
            litt (HsWord64Prim s x) = numericLit "HsWord{64}Prim" x s
            litt (HsIntPrim    s x) = numericLit "HsInt{64}Prim"  x s
            litt (HsInt64Prim  s x) = numericLit "HsInt{64}Prim"  x s
            litt l                  = generic l

            numericLit :: String -> Integer -> SourceText -> SDoc
            numericLit tag x s = braces $ hsep [ text tag
                                               , generic x
                                               , generic s ]

            name :: Name -> SDoc
            name nm    = braces $ text "Name: " <> ppr nm

            occName n  =  braces $
                          text "OccName: "
                       <> text (OccName.occNameString n)

            moduleName :: ModuleName -> SDoc
            moduleName m = braces $ text "ModuleName: " <> ppr m

            srcSpan :: SrcSpan -> SDoc
            srcSpan ss = case b of
             BlankSrcSpan -> text "{ ss }"
             NoBlankSrcSpan -> braces $ char ' ' <>
                             (hang (ppr ss) 1
                                   -- TODO: show annotations here
                                   (text ""))

            var  :: Var -> SDoc
            var v      = braces $ text "Var: " <> ppr v <> text " (" <> ppr (idType v) <> text ")"

            dataCon :: DataCon -> SDoc
            dataCon c  = braces $ text "DataCon: " <> ppr c

            bagRdrName:: Bag (Located (HsBind GhcPs)) -> SDoc
            bagRdrName bg =  braces $
                             text "Bag(Located (HsBind GhcPs)):"
                          $$ (list . bagToList $ bg)

            bagName   :: Bag (Located (HsBind GhcRn)) -> SDoc
            bagName bg  =  braces $
                           text "Bag(Located (HsBind Name)):"
                        $$ (list . bagToList $ bg)

            bagVar    :: Bag (Located (HsBind GhcTc)) -> SDoc
            bagVar bg  =  braces $
                          text "Bag(Located (HsBind Var)):"
                       $$ (list . bagToList $ bg)

            nameSet ns =  braces $
                          text "NameSet:"
                       $$ (list . nameSetElemsStable $ ns)

            fixity :: Fixity -> SDoc
            fixity fx =  braces $
                         text "Fixity: "
                      <> ppr fx

            located :: (Data b,Data loc) => GenLocated loc b -> SDoc
            located (L ss a) = parens $
                   case cast ss of
                        Just (s :: SrcSpan) ->
                          srcSpan s
                        Nothing -> text "nnnnnnnn"
                      $$ showAstData' a

normalize_newlines :: String -> String
normalize_newlines ('\\':'r':'\\':'n':xs) = '\\':'n':normalize_newlines xs
normalize_newlines (x:xs)                 = x:normalize_newlines xs
normalize_newlines []                     = []

