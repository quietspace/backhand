{-# LANGUAGE TemplateHaskell #-}

module Main where

import Backhand
import Control.Concurrent.STM
import Control.Monad
import Data.Map                as Map
import Data.Word
import STMContainers.Map       as STM
import System.Exit
import Test.QuickCheck
import Test.QuickCheck.All     (quickCheckAll)
import Test.QuickCheck.Monadic

-- * Model Testing Library

-- Maybe the 'm' is useless, could just be 'IO'
data Op e x m = Op
    { opName :: String
    , opRun :: e -> x -> PropertyM m x
    }

-- Sorry :(
instance Show (Op e x m) where
    showsPrec p (Op name _) =
        showParen (p > 10) $ showString "Op " . shows name

runOps :: Testable x => IO e -> x -> [Gen (Op e x IO)] -> Property
runOps newEnv model0 operations =
    forAll (listOf $ oneof operations) $
    \ops ->
         monadicIO $
         do env <- run newEnv
            let loop model (Op _ run') = run' env model
            foldM loop model0 ops

-- ** Channel Map Model

newtype ChannelId =
    ChannelId Word64
    deriving (Show,Eq,Ord)

increment :: ChannelId -> ChannelId
increment (ChannelId cid) = ChannelId (cid + 1)

genChannelId :: Gen ChannelId
genChannelId = ChannelId <$> arbitrarySizedBoundedIntegral

data Model = Model
    { mChannels :: Map.Map ChannelId ChanUUID
    , mNext :: ChannelId
    }

instance Testable Model where
  property m = runOps (atomically newChannelMap) m [opAddNew, opDelete]

emptyModel :: Model
emptyModel = Model Map.empty (ChannelId 0)

-- ** Operations

opAddNew :: Gen (Op (ChannelMap c s) Model IO)
opAddNew =
    pure . Op "add-new" $
    \chm (Model cs0 n0) -> do
        uniq <- run $ addNewChannel chm
        sz <- run $ channelMapSize chm
        let cs = Map.insert n uniq cs0
            n = increment n0
        assert $ sz == Map.size cs
        pure $ Model cs n

opDelete :: Gen (Op (ChannelMap c s) Model IO)
opDelete = do
    ix0 <- abs <$> arbitrarySizedBoundedIntegral
    pure . Op ("delete: " ++ show ix0) $
        \chm m0@(Model cs0 n) ->
            if Map.null cs0
                then pure m0
                else do
                    let ix = ix0 `mod` Map.size cs0
                        (cid,uniq) = Map.toList cs0 !! ix
                    run $ delChannel' uniq chm
                    sz <- run $ channelMapSize chm
                    let cs = Map.delete cid cs0
                    assert $ sz == Map.size cs
                    pure $ Model cs n

channelMapSize :: ChannelMap c s -> IO Int
channelMapSize m = atomically $ STM.size m

-- * Tests

prop_test_model :: Property
prop_test_model = property emptyModel

return [] -- Needed for quickcheck TH because of a bug as of GHC 8
tests :: IO Bool
tests = $quickCheckAll

-- * Main

main :: IO ()
main =
  tests >>= \result ->
  if result then exitSuccess else exitFailure
