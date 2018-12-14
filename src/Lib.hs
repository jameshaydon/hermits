module Lib
    ( someFunc
    ) where

import Protolude

someFunc :: IO ()
someFunc = putStrLn ("someFunc" :: Text)
