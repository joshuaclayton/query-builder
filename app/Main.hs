module Main where

import QueryBuilder

main :: IO ()
main = print . parseConstraints =<< getContents
