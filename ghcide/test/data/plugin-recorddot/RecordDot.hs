{-# LANGUAGE DuplicateRecordFields, TypeApplications, TypeFamilies, UndecidableInstances, FlexibleContexts, DataKinds, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}
module RecordDot (Company(..), display) where
data Company = Company {name :: String}
display :: Company -> String
display c = c.name
