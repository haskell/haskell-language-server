{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module TSimpleDecl where
import Language.Haskell.TH ( mkName, clause, normalB, funD, sigD )

-- Foo
--  Bar
foo :: Int
foo = 42
-- Bar
-- ee
-- dddd
