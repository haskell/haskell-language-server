{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module TSimpleDecl where
import Language.Haskell.TH ( mkName, clause, normalB, funD, sigD )

-- Foo
--  Bar
$(sequence
    [sigD (mkName "foo") [t|Int|]
    ,funD (mkName "foo") [clause [] (normalB [|42|]) []]
    ,sigD (mkName "bar") [t|Int|]
    ]
    )
-- Bar
-- ee
-- dddd
