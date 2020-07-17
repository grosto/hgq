module Lib
    ( someFunc
    )
where

import           System.IO                      ( IO
                                                , putStrLn
                                                )


someFunc :: IO ()
someFunc = putStrLn "someFunc"
