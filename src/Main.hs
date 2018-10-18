{-# LANGUAGE BangPatterns #-}

module Main where

main :: IO ()
main = 
  putStrLn "hello world" >>
  putStrLn "Enter something please:" >>
  getLine >>= \(!s) ->
  putStrLn "Writen:" >>
  putStrLn s
