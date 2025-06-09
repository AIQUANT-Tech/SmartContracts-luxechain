{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
 
module NFTMarketEscrowFinal.Contract(validator,validateNFTEscrow,NFTEscrowDatum(..),NFTEscrowAction(..)) where
 
-- import Plutus.V1.Ledger.Interval (contains, from, to)
-- import Plutus.V1.Ledger.Interval (contains, to)
import Plutus.V1.Ledger.Value
  ( flattenValue,
    valueOf,
    adaSymbol,
    adaToken
  )
import Plutus.V2.Ledger.Api
  ( Address (..),
    BuiltinByteString,
    BuiltinData,
    Credential (..),
    -- POSIXTime (..),
    PubKeyHash,
    ScriptContext (..),
    CurrencySymbol,
    TokenName (..),
    -- TxInInfo (..),
    TxInfo (..),
    TxOut (..),
    Validator,
    Value,
    -- fromBuiltin,
    mkValidatorScript,
    scriptContextTxInfo,
    -- toBuiltin,
    txInInfoResolved,
    txInfoInputs,
    txInfoOutputs,
    -- txInfoValidRange,
    txOutAddress,
    txOutValue,
    -- unValidatorScript,
  )
import Plutus.V2.Ledger.Contexts
  ( 
    -- findOwnInput,
    -- getContinuingOutputs,
    txSignedBy,
    valuePaidTo,
    -- valueSpent,
  )
import PlutusTx
  ( compile,
    unsafeFromBuiltinData,
    unstableMakeIsData,
  )
-- import qualified PlutusTx.Builtins as Builtins
import PlutusTx.Prelude
  ( Bool (..),
    Integer,
    any,
    error,
    filter,
    -- fst,
    length,
    -- maybe,
    -- not,
    -- snd,
    traceIfFalse,
    -- ($),
    (&&),
    -- (.),
    -- (<),
    (==),
    (>=),
    (>),
    -- (||),
  )
-- import System.Directory (createDirectoryIfMissing)
-- import System.FilePath (takeDirectory)
import Prelude
  ( 
    -- Either (..),
    -- FilePath,
    -- IO,
    -- Maybe (..),
    Show,
    -- String,
    -- putStrLn,
    -- (+),
    -- (++),
  )
 
-- | Datum structure for the NFT Escrow
data NFTEscrowDatum = NFTEscrowDatum
  { sellerPkh :: PubKeyHash,
    policyId :: CurrencySymbol,
    tokenName :: TokenName,
    price :: Integer,
    hashedOwner   :: BuiltinByteString
  }
  deriving stock (Show)
 
PlutusTx.unstableMakeIsData ''NFTEscrowDatum
 
-- Redeemer for different actions
data NFTEscrowAction
  = Buy
  | Cancel
  deriving stock (Show)
 
PlutusTx.unstableMakeIsData ''NFTEscrowAction
 
-- Helper function to check if a UTxO contains the NFT
-- does this output contain our NFT?
{-# INLINEABLE containsNFT #-}
containsNFT :: TxOut -> NFTEscrowDatum -> Bool
containsNFT txOut dat =
  hasNFTValue (txOutValue txOut) (policyId dat) (tokenName dat)
 
-- Helper to check if a value contains the NFT with given ID
{-# INLINEABLE hasNFTValue #-}
hasNFTValue :: Value -> CurrencySymbol -> TokenName -> Bool
hasNFTValue val cs tn =
  let
    entries = flattenValue val  -- gives [(CurrencySymbol, TokenName, Integer)]
    matches = filter (\(cs',tn',amt) -> cs' == cs && tn' == tn && amt == 1) entries
  in
    length matches > 0
 
{-# INLINEABLE nftReturnedToSeller #-}
nftReturnedToSeller :: TxInfo -> PubKeyHash -> NFTEscrowDatum -> Bool
nftReturnedToSeller txInfo sellerPKH dat =
    any (\o -> containsNFT o dat && isToSeller o) (txInfoOutputs txInfo)
  where
    isToSeller :: TxOut -> Bool
    isToSeller txOut = case txOutAddress txOut of
      Address (PubKeyCredential pkh') _ -> pkh' == sellerPKH
      _ -> False
 
-- Function to check if the input transaction contains the NFT
-- does *any* input to the script contain our NFT?
{-# INLINEABLE scriptHasNFT #-}
scriptHasNFT :: TxInfo -> NFTEscrowDatum -> Bool
scriptHasNFT txInfo dat =
    any (\i -> containsNFT (txInInfoResolved i) dat) (txInfoInputs txInfo)

-- Helper to check if correct amount of ADA is paid to seller
{-# INLINEABLE correctAdaPaymentToSeller #-}
correctAdaPaymentToSeller :: TxInfo -> PubKeyHash -> Integer -> Bool
correctAdaPaymentToSeller txInfo sellerPkhParam requiredAmount =
  let sellerValue = valuePaidTo txInfo sellerPkhParam
      adaAmount = valueOf sellerValue adaSymbol adaToken
   in adaAmount >= requiredAmount
 
-- | Main validation function for the NFT Escrow
{-# INLINEABLE validateNFTEscrow #-}
validateNFTEscrow :: NFTEscrowDatum -> NFTEscrowAction -> ScriptContext -> Bool
validateNFTEscrow datum action ctx =
  let txInfo = scriptContextTxInfo ctx
      signedBySeller = txSignedBy txInfo (sellerPkh datum)
      hasNFT = scriptHasNFT txInfo datum
      nftToSeller = nftReturnedToSeller  txInfo (sellerPkh datum) datum
      correctPaymentToSeller = correctAdaPaymentToSeller txInfo (sellerPkh datum) (price datum)
 
      validateBuy =
        traceIfFalse "Incorrect payment to seller" correctPaymentToSeller
          && traceIfFalse "Script does not have the NFT" hasNFT
 
      validateCancel =
        traceIfFalse "Not signed by seller" signedBySeller
          && traceIfFalse "Script does not have the NFT" hasNFT
          && traceIfFalse "NFT not returned to seller" nftToSeller
 
   in case action of
        Buy -> validateBuy
        Cancel -> validateCancel
 
{-# INLINEABLE wrapValidator #-}
wrapValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrapValidator datum redeemer context =
  if validateNFTEscrow
    (unsafeFromBuiltinData datum)
    (unsafeFromBuiltinData redeemer)
    (unsafeFromBuiltinData context)
    then ()
    else error ()
 
validator :: Validator
validator = mkValidatorScript $$(compile [||wrapValidator||])