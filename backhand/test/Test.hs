module Main where

import Control.Concurrent.STM
import Data.Maybe
import STMContainers.Map      as M
import Test.Tasty
import Test.Tasty.HUnit

import Backhand

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

    , testGroup "BMap sanity"
      [ testCase "Can insert channels into a BMap"
        $ fmap fst bMapInteractionTests
        @? "Couldn't add channel to the BMap"

      , testCase "Can delete a channel from a BMap"
        $ fmap snd bMapInteractionTests
        @? "Couldn't delete the channel from the BMap."
      ]
    ]

subchannelsOnChannelLobby :: IO Bool
subchannelsOnChannelLobby = do
  testLobby <- newLobby []
  pure (isJust $ subchannels testLobby)

subchannelsOnChannels :: IO Bool
subchannelsOnChannels = do
  testRoom <- newRoom []
  pure (isNothing $ subchannels testRoom)

bMapInteractionTests :: IO (Bool, Bool)
bMapInteractionTests = do
  -- spawns a worker thread that will clean up channels over a unagi channel.
  bmap <- newBMap

  -- Inserting a channel that will be cleaned up.
  room <- newRoom []
  insertTest <- atomically $ do
    M.insert room (chanId room) (unBMap bmap)
    M.null (unBMap bmap)

  deleteTest <- atomically $ do
    M.delete (chanId room) (unBMap bmap)
    M.null (unBMap bmap)

  pure (not insertTest, deleteTest)
