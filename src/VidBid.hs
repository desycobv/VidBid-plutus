{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE DuplicateRecordFields      #-}

module VidBid
    (
    VidBIdStateMachineSchema
    , contract
    ) where

import           Control.Monad                hiding (fmap)
import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.Monoid                  (Last (..))
import           Data.Text                    (Text, pack)
import qualified Data.ByteString.Char8        as C
import           Data.Semigroup.Generic       (GenericSemigroupMonoid (..))

import           GHC.Generics                 (Generic)
import           Ledger                       hiding (singleton)
import           Ledger.Ada                   as Ada
import           Ledger.Constraints           as Constraints
import           Ledger.Typed.Tx              (TypedScriptTxOut (..))
import qualified Ledger.Typed.Scripts         as Scripts
import qualified Ledger.Value                 as V

import           Plutus.Contract              as Contract
import           Plutus.Contract.StateMachine as SM
import qualified PlutusTx

import           Data.Aeson                   (FromJSON, ToJSON)
import           GHC.Generics                 (Generic)
import           Schema                       (ToSchema)
import qualified Prelude
import           Prelude                      (Semigroup (..), Show (..), String)
import qualified PlutusTx.Prelude
import           PlutusTx.Prelude              hiding (Semigroup(..), check, unless)

import           VidBidMint

newtype VidId = VidId BuiltinByteString deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData, Show, FromJSON, ToJSON, ToSchema)
PlutusTx.makeLift ''VidId

newtype VidBidTokenValue = VidBidTokenValue Value deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData, Show, FromJSON, ToJSON, ToSchema)
PlutusTx.makeLift ''VidBidTokenValue

newtype MinimalBidValue = MinimalBidValue Value deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData, Show, FromJSON, ToJSON, ToSchema)
PlutusTx.makeLift ''MinimalBidValue

newtype VidOwnerPkh = VidOwnerPkh PubKeyHash deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData, Show, FromJSON, ToJSON, ToSchema)
PlutusTx.makeLift ''VidOwnerPkh

newtype PlatformPkh = PlatformPkh PubKeyHash deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData, Show, FromJSON, ToJSON, ToSchema)
PlutusTx.makeLift ''PlatformPkh

-- create a redeemer script for the guessing VidBId by lifting the
-- string to its on-chain representation
vidIdFromString :: String -> VidId
vidIdFromString = VidId . toBuiltin . C.pack

data HighestBid =
    HighestBid
        { highestBid    :: Value
        , highestBidder :: PubKeyHash
        }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''HighestBid
PlutusTx.makeLift ''HighestBid


-- | State of the guessing vidBId
-- | State of the guessing vidBId
data VidBidState =
    Initialised PlatformPkh VidBidTokenValue VidOwnerPkh
    -- ^ Initial state. In this state only the 'MintTokens' action is allowed.
    | Opened PlatformPkh VidBidTokenValue VidOwnerPkh MinimalBidValue
    | Offered PlatformPkh VidBidTokenValue VidOwnerPkh HighestBid
    | Closed PlatformPkh VidBidTokenValue
    -- ^ Funds have been locked. In this state only the 'Guess' action is
    --   allowed.
    | Destroyed
    -- ^ All funds were unlocked.
    deriving stock (Prelude.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''VidBidState
PlutusTx.makeLift ''VidBidState

-- | Observable state of the auction app
data VidBidOutput =
    VidBidOutput
        { vidbidState        :: Last VidBidState
        }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

deriving via (GenericSemigroupMonoid VidBidOutput) instance (Prelude.Semigroup VidBidOutput)
deriving via (GenericSemigroupMonoid VidBidOutput) instance (Prelude.Monoid VidBidOutput)

vidbidStateOut :: VidBidState -> VidBidOutput
vidbidStateOut s = Prelude.mempty  { vidbidState = Last (Just s) }

-- | Inputs (actions)
data VidBIdInput =
    MintToken
    | Open {ownerPkh :: PubKeyHash, minBidValue :: Value}
    | Bid { newBid :: Value, newBidder :: PubKeyHash }
    | Payday {paydayValue :: Value}
    | Grab {ownerPkh :: PubKeyHash}
    | Destroy
    deriving stock (Prelude.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)
PlutusTx.unstableMakeIsData ''VidBIdInput
PlutusTx.makeLift ''VidBIdInput


{-# INLINABLE transition #-}
transition :: VidId -> State VidBidState -> VidBIdInput -> Maybe (TxConstraints Void Void, State VidBidState)
transition _ State{stateData=oldData, stateValue=oldValue} input = case (oldData, input) of
--  MintToken
    (Initialised (PlatformPkh platformPkh) (VidBidTokenValue tokenVal) (VidOwnerPkh ownerPkh), MintToken ) ->
        let constraints = Constraints.mustMintValue tokenVal <>
                          Constraints.mustPayToPubKey ownerPkh tokenVal <>
                          Constraints.mustBeSignedBy platformPkh
        in
        Just ( constraints
             , State
                { stateData = Closed (PlatformPkh platformPkh) (VidBidTokenValue tokenVal)
                , stateValue = oldValue
                }
             )
--    OPEN
    (Closed platformPkh (VidBidTokenValue tokenVal), Open{ownerPkh, minBidValue}) ->
        let constraints = Constraints.mustPayToPubKey ownerPkh oldValue
        in
        Just ( constraints
             , State
                { stateData = Opened platformPkh (VidBidTokenValue tokenVal) (VidOwnerPkh ownerPkh) (MinimalBidValue minBidValue)
                , stateValue = tokenVal
                }
             )
--    BID
    (Opened platformPkh (VidBidTokenValue tokenVal) ownerPkh (MinimalBidValue minValue) , Bid{ newBid, newBidder})
     | isValidBidValue minValue newBid ->
        let constraints = mempty
        in
        Just ( constraints
             , State
                { stateData = Offered platformPkh (VidBidTokenValue tokenVal) ownerPkh HighestBid{highestBid = newBid, highestBidder = newBidder}
                , stateValue = oldValue + newBid
                }
             )
    (Offered platformPkh (VidBidTokenValue tokenVal) ownerPkh (HighestBid currentValue currentBidderPkh) , Bid{ newBid, newBidder})
     | isValidBidValue currentValue newBid ->
        let constraints = Constraints.mustPayToPubKey currentBidderPkh currentValue
        in
        Just ( constraints
             , State
                { stateData = Offered platformPkh (VidBidTokenValue tokenVal) ownerPkh HighestBid{highestBid = newBid, highestBidder = newBidder}
                , stateValue = tokenVal + newBid
                }
             )
--     PAYDAY
    (Opened (PlatformPkh platformPkh) (VidBidTokenValue tokenVal) (VidOwnerPkh ownerPkh) _ , Payday{paydayValue})
     | scriptContainsToken oldValue tokenVal ->
        let constraints = Constraints.mustPayToPubKey ownerPkh tokenVal <>
                          Constraints.mustBeSignedBy platformPkh
        in
          Just ( constraints
             , State
                { stateData = Closed (PlatformPkh platformPkh) (VidBidTokenValue tokenVal)
                , stateValue = paydayValue
                }
             )
    (Offered (PlatformPkh platformPkh) (VidBidTokenValue tokenVal) (VidOwnerPkh currentOwnerPkh) (HighestBid bidValue bidderPkh) , Payday{paydayValue} )
     | scriptContainsToken oldValue tokenVal ->
        let constraints = Constraints.mustPayToPubKey currentOwnerPkh bidValue <>
                          Constraints.mustPayToPubKey bidderPkh tokenVal <>
                          Constraints.mustBeSignedBy platformPkh
        in
          Just ( constraints
               , State
                  { stateData = Closed  (PlatformPkh platformPkh) (VidBidTokenValue tokenVal)
                  , stateValue = paydayValue
                  }
               )
    (Closed (PlatformPkh platformPkh) (VidBidTokenValue tokenVal), Payday{paydayValue} )
     | scriptContainsToken oldValue tokenVal ->
        let constraints = Constraints.mustBeSignedBy platformPkh
        in
          Just ( constraints
               , State
                  { stateData = Closed (PlatformPkh platformPkh) (VidBidTokenValue tokenVal)
                  , stateValue = paydayValue
                  }
               )
--    Grab
    (Closed platformPkh (VidBidTokenValue tokenVal), Grab{ownerPkh} ) ->
        let constraints = Constraints.mustSpendAtLeast tokenVal <>
                          Constraints.mustPayToPubKey ownerPkh oldValue
        in
          Just ( constraints
               , State
                  { stateData = Closed platformPkh (VidBidTokenValue tokenVal)
                  , stateValue = Ada.lovelaceValueOf 0
                  }
               )
--    Destroy
    (Initialised (PlatformPkh platformPkh) _ _ ,  Destroy ) ->
        let constraints = Constraints.mustBeSignedBy platformPkh
        in
          Just ( constraints
               , State
                  { stateData = Destroyed
                  , stateValue = Ada.lovelaceValueOf 0
                  }
               )
    (Closed (PlatformPkh platformPkh) _ ,  Destroy ) ->
        let constraints = Constraints.mustBeSignedBy platformPkh
        in
          Just ( constraints
               , State
                  { stateData = Destroyed
                  , stateValue = Ada.lovelaceValueOf 0
                  }
               )
    (Opened (PlatformPkh platformPkh) _ (VidOwnerPkh ownerPkh) _ ,  Destroy ) ->
        let constraints = Constraints.mustBeSignedBy platformPkh <>
                          Constraints.mustPayToPubKey ownerPkh oldValue
        in
          Just ( constraints
               , State
                  { stateData = Destroyed
                  , stateValue = Ada.lovelaceValueOf 0
                  }
               )
    (Offered (PlatformPkh platformPkh) (VidBidTokenValue tokenVal) (VidOwnerPkh ownerPkh) (HighestBid bidValue bidderPkh) ,  Destroy ) ->
        let constraints = Constraints.mustBeSignedBy platformPkh <>
                          Constraints.mustPayToPubKey ownerPkh tokenVal <>
                          Constraints.mustPayToPubKey bidderPkh bidValue
        in
          Just ( constraints
               , State
                  { stateData = Destroyed
                  , stateValue = Ada.lovelaceValueOf 0
                  }
               )

--Nothing matches
    _ -> Nothing


{-# INLINABLE isValidBidValue #-}
-- | Check whether a proposed 'Payment' is valid given the total
--   amount of funds currently locked in the contract.
isValidBidValue :: Value -> Value -> Bool
isValidBidValue old new = Ada.fromValue old < Ada.fromValue new

{-# INLINABLE scriptContainsToken #-}
-- | Check whether a proposed 'Payment' is valid given the total
--   amount of funds currently locked in the contract.
scriptContainsToken :: Value -> Value -> Bool
scriptContainsToken old token = V.geq old token

type VidBIdStateMachine = SM.StateMachine VidBidState VidBIdInput

{-# INLINABLE machine #-}
machine :: VidId ->  StateMachine VidBidState VidBIdInput
machine vidId = SM.mkStateMachine Nothing (transition vidId) isFinal where
    isFinal Destroyed = True
    isFinal _         = False

{-# INLINABLE mkVidBidValidator #-}
mkVidBidValidator :: VidId -> Scripts.ValidatorType VidBIdStateMachine
mkVidBidValidator vidId = SM.mkValidator (machine vidId)

typedVidbidValidator :: VidId -> Scripts.TypedValidator VidBIdStateMachine
typedVidbidValidator = Scripts.mkTypedValidatorParam @VidBIdStateMachine
    $$(PlutusTx.compile [|| mkVidBidValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator

client :: VidId -> SM.StateMachineClient VidBidState VidBIdInput
client vidId = SM.mkStateMachineClient $ SM.StateMachineInstance (machine vidId) (typedVidbidValidator vidId)

data InitArgs = InitArgs
    { vidId            :: String
    , platformPkh      :: PubKeyHash
    }
    deriving stock ( Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)

init :: ( AsContractError e
            , AsSMContractError e
            ) => Promise VidBidOutput VidBIdStateMachineSchema e (Maybe VidBidOutput)
init = endpoint @"init" @InitArgs $ \(InitArgs vidId platformPkhParam) -> do
    logInfo  @String $ "Initialise with vidId: " ++ vidId
    pkh         <- Contract.ownPubKeyHash
    let initVidId          = V.TokenName (toBuiltin (C.pack vidId))
        tokenVal           = VidBidTokenValue (VidBidMint.getTokenValue platformPkhParam initVidId)
        ownerPkh           = VidOwnerPkh pkh
        platformPkh        = PlatformPkh platformPkhParam
    void $ SM.runInitialise (client (vidIdFromString vidId)) (Initialised platformPkh tokenVal ownerPkh) mempty
    maybeState <- SM.getOnChainState (client (vidIdFromString vidId))
    case maybeState of
      Just (onChainState, _)  ->
        do
          let OnChainState{ocsTxOut=TypedScriptTxOut{tyTxOutData=valueInState}} = onChainState
          logInfo @String $ "VidBid contract found in state: " ++ show valueInState
          tell $ vidbidStateOut $ valueInState
          pure (Just (vidbidStateOut $ valueInState))
      Nothing -> do
        logError @String $ "Couldn't find state of contract."
        pure Nothing

data MintArgs = MintArgs
    { vidId            :: String
    }
    deriving stock ( Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)

mintToken :: ( AsContractError e
            , AsSMContractError e
            ) => Promise VidBidOutput VidBIdStateMachineSchema e (Maybe VidBidOutput)
mintToken = endpoint @"mint" @MintArgs $ \(MintArgs vidId) -> do
    logInfo  @String $ "Mint token for vidId: " ++ vidId
    pkh         <- Contract.ownPubKeyHash
    let lookups   = Constraints.mintingPolicy (policy pkh)
        theClient =  client (vidIdFromString vidId)
    logInfo @String $ "Platorm pkh: " ++ show pkh
    void $ SM.runStepWith lookups mempty theClient (MintToken)
    maybeState <- SM.getOnChainState (client (vidIdFromString vidId))
    case maybeState of
      Just (onChainState, _)  ->
        do
          let OnChainState{ocsTxOut=TypedScriptTxOut{tyTxOutData=valueInState}} = onChainState
          logInfo @String $ "VidBid contract found in state: " ++ show valueInState
          tell $ vidbidStateOut $ valueInState
          pure (Just (vidbidStateOut $ valueInState))
      Nothing -> do
        logError @String $ "Couldn't find state of contract."
        pure Nothing

data OpenArgs = OpenArgs
    { vidId     :: String
    , minPrice  :: Integer
    }
    deriving stock ( Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)

open :: ( AsContractError e
            , AsSMContractError e
            ) => Promise VidBidOutput VidBIdStateMachineSchema e (Maybe VidBidOutput)
open = endpoint @"open" @OpenArgs $ \(OpenArgs vidId minPrice) -> do
    logInfo  @String $ "Open auction for vidId: " ++  vidId
    pkh         <- Contract.ownPubKeyHash
    let minValue = Ada.lovelaceValueOf minPrice
    void $ SM.runStep (client (vidIdFromString vidId)) Open{ownerPkh = pkh, minBidValue = minValue}
    maybeState <- SM.getOnChainState (client (vidIdFromString vidId))
    case maybeState of
      Just (onChainState, _)  ->
        do
          let OnChainState{ocsTxOut=TypedScriptTxOut{tyTxOutData=valueInState}} = onChainState
          logInfo @String $ "VidBid contract found in state: " ++ show valueInState
          tell $ vidbidStateOut $ valueInState
          pure (Just (vidbidStateOut $ valueInState))
      Nothing -> do
        logError @String $ "Couldn't find state of contract."
        pure Nothing

data BidArgs = BidArgs
    { vidId     :: String
    , bidPrice  :: Integer
    }
    deriving stock ( Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)

bid :: ( AsContractError e
            , AsSMContractError e
            ) => Promise VidBidOutput VidBIdStateMachineSchema e (Maybe VidBidOutput)
bid = endpoint @"bid" @BidArgs $ \(BidArgs vidId bidPrice) -> do
    logInfo  @String $ "Bid on vidId: " ++ vidId
    pkh         <- Contract.ownPubKeyHash
    let bidValue = Ada.lovelaceValueOf bidPrice
    void $ SM.runStep (client (vidIdFromString vidId))  Bid{newBid = bidValue, newBidder = pkh}
    maybeState <- SM.getOnChainState (client (vidIdFromString vidId))
    case maybeState of
      Just (onChainState, _)  ->
        do
          let OnChainState{ocsTxOut=TypedScriptTxOut{tyTxOutData=valueInState}} = onChainState
          logInfo @String $ "VidBid contract found in state: " ++ show valueInState
          tell $ vidbidStateOut $ valueInState
          pure (Just (vidbidStateOut $ valueInState))
      Nothing -> do
        logError @String $ "Couldn't find state of contract."
        pure Nothing

data PaydayArgs = PaydayArgs
    { vidId     :: String
    , adaValue  :: Integer
    }
    deriving stock ( Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)

payday :: ( AsContractError e
            , AsSMContractError e
            ) => Promise VidBidOutput VidBIdStateMachineSchema e (Maybe VidBidOutput)
payday = endpoint @"payday" @PaydayArgs $ \(PaydayArgs vidId adaValue) -> do
    logInfo  @String $ "PayDay!!! for vidId: " ++ vidId
    let paydayValue = Ada.lovelaceValueOf adaValue
    void $ SM.runStep (client (vidIdFromString vidId)) Payday{paydayValue = paydayValue}
    maybeState <- SM.getOnChainState (client (vidIdFromString vidId))
    case maybeState of
      Just (onChainState, _)  ->
        do
          let OnChainState{ocsTxOut=TypedScriptTxOut{tyTxOutData=valueInState}} = onChainState
          logInfo @String $ "VidBid contract found in state: " ++ show valueInState
          tell $ vidbidStateOut $ valueInState
          pure (Just (vidbidStateOut $ valueInState))
      Nothing -> do
        logError @String $ "Couldn't find state of contract."
        pure Nothing

data GrabArgs = GrabArgs
    { vidId     :: String
    }
    deriving stock ( Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)

grab :: ( AsContractError e
            , AsSMContractError e
            ) => Promise VidBidOutput VidBIdStateMachineSchema e (Maybe VidBidOutput)
grab = endpoint @"grab" @GrabArgs $ \(GrabArgs vidId) -> do
    logInfo  @String $ "Grabbing fees of vidId: " ++ vidId
    pkh         <- Contract.ownPubKeyHash
    void $ SM.runStep (client (vidIdFromString vidId)) Grab{ownerPkh = pkh}
    maybeState <- SM.getOnChainState (client (vidIdFromString vidId))
    case maybeState of
      Just (onChainState, _)  ->
        do
          let OnChainState{ocsTxOut=TypedScriptTxOut{tyTxOutData=valueInState}} = onChainState
          logInfo @String $ "VidBid contract found in state: " ++ show valueInState
          tell $ vidbidStateOut $ valueInState
          pure (Just (vidbidStateOut $ valueInState))
      Nothing -> do
        logError @String $ "Couldn't find state of contract."
        pure Nothing

data DestroyArgs = DestroyArgs
    { vidId     :: String
    }
    deriving stock ( Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)

destroy :: ( AsContractError e
            , AsSMContractError e
            ) => Promise VidBidOutput VidBIdStateMachineSchema e (Maybe VidBidOutput)
destroy = endpoint @"destroy" @DestroyArgs $ \(DestroyArgs vidId) -> do
    logInfo  @String $ "Destroy instance for vidId: " ++ vidId
    void $ SM.runStep (client (vidIdFromString vidId)) Destroy
    maybeState <- SM.getOnChainState (client (vidIdFromString vidId))
    case maybeState of
      Just (onChainState, _)  ->
        do
          let OnChainState{ocsTxOut=TypedScriptTxOut{tyTxOutData=valueInState}} = onChainState
          logInfo @String $ "VidBid contract found in state: " ++ show valueInState
          tell $ vidbidStateOut $ valueInState
          pure (Just (vidbidStateOut $ valueInState))
      Nothing -> do
        logError @String $ "Couldn't find state of contract."
        pure Nothing

data LookupArgs = LookupArgs
    { vidId     :: String
    }
    deriving stock ( Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)

lookup :: ( AsContractError e
            , AsSMContractError e
            ) => Promise VidBidOutput VidBIdStateMachineSchema e (Maybe VidBidOutput)
lookup = endpoint @"lookup" @LookupArgs $ \(LookupArgs vidId) -> do
  logInfo  @String $ "Fetching contract state for vidId: " ++ vidId
  maybeState <- SM.getOnChainState (client (vidIdFromString vidId))
  case maybeState of
    Just (onChainState, _)  ->
      do
        let OnChainState{ocsTxOut=TypedScriptTxOut{tyTxOutData=valueInState}} = onChainState
        logInfo @String $ "VidBid contract found in state: " ++ show valueInState
        tell $ vidbidStateOut $ valueInState
        pure (Just (vidbidStateOut $ valueInState))
    Nothing -> do
      logError @String $ "Couldn't find state of contract."
      pure Nothing

-- | The schema of the contract. It consists of the two endpoints @"lock"@
--   and @"guess"@ with their respective argument types.
type VidBIdStateMachineSchema =
        Endpoint "init" InitArgs
        .\/ Endpoint "mint" MintArgs
        .\/ Endpoint "lookup" LookupArgs
        .\/ Endpoint "open" OpenArgs
        .\/ Endpoint "bid" BidArgs
        .\/ Endpoint "payday" PaydayArgs
        .\/ Endpoint "grab" GrabArgs
        .\/ Endpoint "destroy" DestroyArgs

contract :: ( AsContractError e
                 , AsSMContractError e
                 ) => Contract VidBidOutput VidBIdStateMachineSchema e (Maybe VidBidOutput)
contract = do
    selectList [init, mintToken, lookup, open, bid, payday, grab, destroy] >> contract


