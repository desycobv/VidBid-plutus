{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE RecordWildCards            #-}

module VidBidMint (   policy
                   , mint
                   , curSymbol
                   , getTokenValue
                   , vidBidMintContract
                   , VidBidMintSchema
                   , CreateParams(..)
                  )
                  where

import           Control.Monad          hiding (fmap)
import           Data.Text              (Text)
import           Data.Void              (Void)
import           Plutus.Contract        as Contract
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (mint, singleton)
import           Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import           Prelude                (Show (..),Semigroup (..), String)
import qualified Prelude                as Haskell

import           Text.Printf            (printf)
import           Data.Aeson             (FromJSON, ToJSON)
import           Data.Semigroup         (Last (..))
import           GHC.Generics           (Generic)
import           Schema                 (ToSchema)

data CreateParams = CreateParams
    { cpTokenName :: TokenName
    , cpOwnerPkh  :: PubKeyHash
    }
    deriving stock (Generic, Haskell.Show, Haskell.Eq)
    deriving anyclass (FromJSON, ToJSON, ToSchema)


{-# INLINABLE mkPolicy #-}
mkPolicy :: PubKeyHash -> () -> ScriptContext -> Bool
mkPolicy pkh () ctx =  traceIfFalse "wrong amount minted" checkMintedAmountIsOne &&
                       traceIfFalse "Not signed by correct party" (txSignedBy (scriptContextTxInfo ctx) pkh)
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    checkMintedAmountIsOne :: Bool
    checkMintedAmountIsOne = case flattenValue (txInfoMint  info) of
        [(_, _, amt)] -> amt == 1
        _                -> False

policy :: PubKeyHash -> Scripts.MintingPolicy
policy pkh = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \pkh' -> Scripts.wrapMintingPolicy $ mkPolicy pkh' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode pkh

curSymbol :: PubKeyHash -> CurrencySymbol
curSymbol pkh = scriptCurrencySymbol $ policy pkh

getTokenValue :: PubKeyHash -> TokenName -> Value
getTokenValue pkh tn = Value.singleton (curSymbol pkh) tn 1

type VidBidMintSchema =
          Endpoint "mint" CreateParams

mint :: (AsContractError e) => Promise () VidBidMintSchema e ()
mint = endpoint @"mint" @CreateParams $ \(cp) -> do
    pkh         <- Contract.ownPubKeyHash
    let tn       = cpTokenName cp
        ownerPkh = cpOwnerPkh  cp
        val      = Value.singleton (curSymbol pkh) tn 1
        lookups  = Constraints.mintingPolicy (policy pkh)
        tx       = Constraints.mustMintValue val
                    <> Constraints.mustBeSignedBy pkh
                      <> Constraints.mustPayToPubKey ownerPkh val
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    _ <- awaitTxConfirmed (getCardanoTxId ledgerTx)
    Contract.logInfo @String $ printf "forged %s" (show val)



vidBidMintContract :: AsContractError e => Contract () VidBidMintSchema Text e
vidBidMintContract = do
    selectList [mint] >> vidBidMintContract