{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Useful functions for implementing rooms where clients can join a game as a
-- player.
module Backhand.Behavior.Players
    ( PlayerSlot
    , PlayerSlotList
    , playerMessages
    , sendPlayerMsgs
    , identSlots
    , getSlot
    ) where

import Prelude hiding (id, (.))

import Control.Auto
import Control.Auto.Blip
import Control.Auto.Blip.Internal
import Control.Auto.Switch
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Bimap as BM
import qualified Data.Text as T

import Backhand.Room


-- | Represents a "player slot". At any given time, a player slot may be
-- connected to a single client, from which the slot can receive messages to be
-- passed on to the game. The mechanism by which a player connects to a slot is
-- determined by the functions that set up the slot.
type PlayerSlot = Maybe Client


-- | Outputs messages from the given client only.
fromClient :: PlayerSlot -> RoomAuto (Blip MsgData)
fromClient Nothing = never
fromClient (Just c) = mapMaybeB match <<< clientMsg
  where
    match :: ClientMsg -> Maybe MsgData
    match (ClientMsg c' msg) =
        if c == c' then Just msg else Nothing


-- | An auto which outputs messages from the given player slot.
playerMessages :: Auto' (PlayerSlot, ClientEvent) (Blip MsgData)
playerMessages = proc (clientM, evt) -> do
  clientChangeB <- onChange_ -< clientM
  switchOnF_ fromClient never -< (evt, clientChangeB)


-- | Takes a player slot and a blip stream of encodeable messages and produces a
-- stream which will send the messages to whatever client is currently connected
-- to the player slot.
sendPlayerMsgs :: EncodeMsg a => Auto' (PlayerSlot, Blip a) [(Client, MsgData)]
sendPlayerMsgs = proc (clientM, msg) -> do
  let msgD = encodeMsg <$> msg

  -- Probably reaking blip semantics here. Hope the universe doesn't implode...
  let msgM = blip Nothing Just msgD

  let msgTuple :: Maybe (Client, MsgData)
      msgTuple = (,) <$> clientM <*> msgM

  msgTupleB <- onJusts -< msgTuple
  sendMessages -< msgTupleB


-- | Parses join game requests for an `identSlot` with the given ID.
identJoinReqs :: (FromJSON a, Eq a, Ord a) => RoomAuto (Blip (a, Client))
identJoinReqs = mapMaybeB (parseMaybe parseJoin) <<< clientMsg
  where
    parseJoin (ClientMsg c (MsgData "join-game" v)) =
        (, c) <$> (v .: "slot")
    parseJoin _ = mzero


-- | Produces a list of player slots with a given set of identifiers. When a
-- slot is empty, players can join by sending a `"join-game"` message with a
-- `slot` field in the JSON containing this player slot's ID. Outputs tuple with
-- a map of slot IDs to player slots and a blip stream of messages to send to
-- clients. This blip stream contains messages related to the slot, such as
-- error messages sent to clients who try to join a slot that is already
-- occupied.
identSlots :: (ToJSON a, FromJSON a, Eq a, Ord a) =>
              RoomAuto (PlayerSlotList a, [(Client, MsgData)])
identSlots = proc evt -> do
  rec
    joinReqB <- identJoinReqs -< evt

    (slotList, slotEvt) <- playerSlotList -< (joinReqB, evt)

    let evtWithClientB = (slotEvt, ) <$> snd <$> joinReqB
    clientMsgB <- mapMaybeB sendClientMsg -< evtWithClientB

    updateMsgB <- emitJusts sendUpdate -< slotEvt

    clients <- clientList -< evt
    let sendUpdateB :: Blip [(Client, MsgData)]
        sendUpdateB = (\msg -> map (, msg) clients) <$> updateMsgB

    clientMsgs <- sendMessages -< clientMsgB
    updateMsgs <- sendMultiMessages -< sendUpdateB
  id -< (slotList, clientMsgs ++ updateMsgs)

-- | A map of slot IDs to their associated clients.
type PlayerSlotList a = BM.Bimap a Client

-- | Gets the slot with the given ID from the given slot list.
getSlot :: (FromJSON a, Eq a, Ord a) =>
           a -> Auto' (PlayerSlotList a) PlayerSlot
getSlot pId = arr (BM.lookup pId)


-- | Sends an error message to the appropriate client if the given slot event is
-- an error.
sendClientMsg :: ToJSON a => (PlayerSlotEvent a, Client) -> Maybe (Client, MsgData)
sendClientMsg (SlotOccupied, c) = Just (c, msg)
  where msg = msgData "error" [ "msg" .= String "Someone has already joined as that player." ]
sendClientMsg (AlreadyJoined, c) = Just (c, msg)
    where msg = msgData "error" [ "msg" .= String "You have already joined the game." ]
sendClientMsg (JoinedSlot sId c, c')
    | c == c'= Just (c, msg)
    | otherwise = Nothing
    where msg = msgData "you-joined" [ "player" .= sId ]
sendClientMsg _ = Nothing


-- | Generates update messages to send to all clients to let them know who has
-- joined what player slot.
sendUpdate :: ToJSON a => PlayerSlotEvent a -> Maybe MsgData
sendUpdate (JoinedSlot sId c) = Just $
    msgData "slot-joined"
            [ "client" .= String (T.pack $ show c)
            , "slot" .= sId
            ]
sendUpdate (LeftSlot sId _) = Just $
    msgData "slot-left"
            [ "slot" .= sId ]
sendUpdate _ = Nothing


-- | Data type representing the `playerSlotList` auto's responses to particular
-- things happening.
data PlayerSlotEvent a
    = SlotOccupied
    | AlreadyJoined
    | JoinedSlot a Client
    | LeftSlot a Client
    | NoSlotEvent


-- | Auto which takes a stream of join requests and a stream of client events
-- and outputs a `PlayerSlotList` and a blip stream of player slot events.
playerSlotList :: forall a. (FromJSON a, Eq a, Ord a) =>
                  Auto' (Blip (a, Client), ClientEvent)
                        (PlayerSlotList a, PlayerSlotEvent a)
playerSlotList = mkState_ func BM.empty
  where
    func :: (Blip (a, Client), ClientEvent) -> PlayerSlotList a
         -> ((PlayerSlotList a, PlayerSlotEvent a), PlayerSlotList a)
    func (Blip (pId, client), _) psMap
        -- Fail if either the client has already joined a slot or another
        -- client has already joined this slot.
        | BM.memberR client psMap = ((psMap, alreadyJoined), psMap)
        | BM.member  pId    psMap = ((psMap, slotOccupied),  psMap)
        | otherwise = ((newMap, joined pId client), newMap)
        where newMap = BM.insert pId client psMap
    func (_, ClientPartEvt client) psMap
        | BM.memberR client psMap =
            ((newMap, playerLeft (psMap BM.!> client) client), newMap)
        | otherwise = ((psMap, noEvt), psMap)
        where newMap = BM.deleteR client psMap
    func _ psMap = ((psMap, noEvt), psMap)

    noEvt = NoSlotEvent
    slotOccupied = SlotOccupied
    alreadyJoined = AlreadyJoined
    joined pId client = JoinedSlot pId client
    playerLeft pId client = LeftSlot pId client
