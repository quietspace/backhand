module Main where

import           Data.Maybe
import           Test.Tasty
import           Test.Tasty.HUnit

import           Backhand

main :: IO ()
main = defaultMain $
  testGroup "Backhand testing"
    [ testGroup "Channel sanity"
      [ testCase "subchannels on Lobby returns a Just"
        $ subchannelsOnChannelLobby
        @? "subchannels on Lobby returns a Nothing"

      , testCase "subchannels on Room returns a Nothing"
        $ subchannelsOnChannels
        @? "subchannels on Room returns a Just"
      ]

    , testGroup "Thread spawning"
      [ testCase "Spawning Backhand"
        $ testSpawnBackhand
        @? "spawnBackhand doesn't spawn correctly with a blank BMap"
      ]
    ]

subchannelsOnChannelLobby :: IO Bool
subchannelsOnChannelLobby = do
  testLobby <- mkEmptyLobby (UIDInteger 00)
  return (isJust $ subchannels testLobby)

subchannelsOnChannels :: IO Bool
subchannelsOnChannels = do
  testRoom <- mkEmptyRoom (UIDInteger 00)
  return (isNothing $ subchannels testRoom)

testSpawnBackhand :: IO Bool
testSpawnBackhand = do
  backhand <- spawnBackhand
  return True
