-- {-# LANGUAGE OverloadedStrings #-}

import ServerLib

main :: IO ()
main = defaultMain pre post
  where
    pre _ = pure ()
    post _ = pure ()
