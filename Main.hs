module Main where

import Network.WebSockets

import Backhand.Core
import Backhand.Connection

main :: IO ()
main = do
    core <- initCore
    runServer "127.0.0.1" 4242 $ websockHandler core
