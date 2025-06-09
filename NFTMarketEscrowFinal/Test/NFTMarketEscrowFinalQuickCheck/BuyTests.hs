{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NumericUnderscores #-}

module Main where

import Prelude
  ( String, IO, ($), (.), (<$>), return, Maybe(..), mempty, Bool(..), Eq(..)
  , Int, Integer,(<), (>), head, (-), (+), (/=)
  )
import Test.Tasty                ( defaultMain, testGroup, TestTree )
import Test.Tasty.QuickCheck     ( testProperty, Property )
import Test.QuickCheck           ( Arbitrary(..), choose, vectorOf, Gen, Positive(..)
                                 , forAll, (===), (==>) )

import Data.String                ( IsString(..) )

import qualified Data.ByteString.Char8 as BS
import PlutusTx.Builtins.Class (stringToBuiltinByteString)
import Plutus.V1.Ledger.Interval  ( to )
import PlutusTx.Builtins (BuiltinByteString)
import Plutus.V2.Ledger.Api
  ( PubKeyHash(..), CurrencySymbol(..), TokenName(..)
  , ScriptContext(..), TxInfo(..), TxOut(..), TxInInfo(..), TxOutRef(..)
  , Address(..), Credential(..), OutputDatum(..), ScriptPurpose(..), ValidatorHash(..)
  )
import Plutus.V1.Ledger.Value    ( singleton )
import qualified PlutusTx.AssocMap as AssocMap

import qualified NFTMarketEscrowFinal.Contract
  ( validateNFTEscrow, NFTEscrowDatum(..), NFTEscrowAction(..)
  , sellerPkh, policyId, tokenName, price
  )

--------------------------------------------------------------------------------
-- Dummy hash (for script UTxO address)
dummyHash :: ValidatorHash
dummyHash = ValidatorHash "00deadbeefdeadbeefdeadbeefdeadbeef"

--------------------------------------------------------------------------------
-- Arbitrary instances for QuickCheck

instance Arbitrary PubKeyHash where
  arbitrary = PubKeyHash . fromString <$> vectorOf 8 (choose ('a','f'))

instance Arbitrary CurrencySymbol where
  arbitrary = CurrencySymbol . fromString <$> vectorOf 8 (choose ('a','f'))

instance Arbitrary TokenName where
  arbitrary = TokenName . fromString <$> vectorOf 4 (choose ('A','Z'))

instance Arbitrary NFTMarketEscrowFinal.Contract.NFTEscrowDatum where
  arbitrary = do
    seller <- arbitrary
    policy <- arbitrary
    name   <- arbitrary
    priceV <- choose (1, 20_000_000)
    dummyOwnerHash <- stringToBuiltinByteString <$> vectorOf 8 (choose ('a','f'))
    return $ NFTMarketEscrowFinal.Contract.NFTEscrowDatum
      { NFTMarketEscrowFinal.Contract.sellerPkh = seller
      , NFTMarketEscrowFinal.Contract.policyId  = policy
      , NFTMarketEscrowFinal.Contract.tokenName = name
      , NFTMarketEscrowFinal.Contract.price     = priceV
      , NFTMarketEscrowFinal.Contract.hashedOwner = dummyOwnerHash
      }

--------------------------------------------------------------------------------
-- Build a simple ScriptContext

mkContext
  :: [TxOut]               -- ^ Script inputs (we use head)
  -> [TxOut]               -- ^ Outputs
  -> NFTMarketEscrowFinal.Contract.NFTEscrowDatum
  -> [PubKeyHash]          -- ^ Signatories
  -> ScriptContext
mkContext inputs outputs _dat signers =
  ScriptContext
    (TxInfo
      { txInfoInputs          = [ TxInInfo (TxOutRef "txhash" 0) (head inputs) ]
      , txInfoReferenceInputs = []
      , txInfoOutputs         = outputs
      , txInfoFee             = mempty
      , txInfoMint            = mempty
      , txInfoDCert           = []
      , txInfoWdrl            = AssocMap.empty
      , txInfoSignatories     = signers
      , txInfoRedeemers       = AssocMap.empty
      , txInfoData            = AssocMap.empty
      , txInfoId              = "dummy"
      })
    (Spending (TxOutRef "txhash" 0))

-- Create a UTxO that carries exactly one NFT matching the datum
mkScriptInput
  :: NFTMarketEscrowFinal.Contract.NFTEscrowDatum
  -> TxOut
mkScriptInput dat =
  TxOut
    (Address (ScriptCredential dummyHash) Nothing)
    (singleton (NFTMarketEscrowFinal.Contract.policyId dat)
               (NFTMarketEscrowFinal.Contract.tokenName dat)
               1)
    (OutputDatumHash "")
    Nothing

-- A UTxO with no NFT (for failure cases)
mkNonNFTInput :: TxOut
mkNonNFTInput =
  TxOut
    (Address (ScriptCredential dummyHash) Nothing)
    mempty
    (OutputDatumHash "")
    Nothing

-- Pay a given lovelace amount to any PubKeyHash
mkAdaOut :: Address -> Integer -> TxOut
mkAdaOut addr adaAmt = TxOut addr (singleton "" "" adaAmt) NoOutputDatum Nothing

--------------------------------------------------------------------------------
-- Properties for Buy

-- | Buy succeeds if the script input has exactly one NFT, and payment == price
prop_buy_success_exact
  :: NFTMarketEscrowFinal.Contract.NFTEscrowDatum
  -> Property
prop_buy_success_exact dat =
  let
    scriptIn   = mkScriptInput dat
    sellerAddr = Address (PubKeyCredential (NFTMarketEscrowFinal.Contract.sellerPkh dat)) Nothing
    payOut     = mkAdaOut sellerAddr (NFTMarketEscrowFinal.Contract.price dat)

    ctx = mkContext [scriptIn] [payOut] dat []
  in
    NFTMarketEscrowFinal.Contract.validateNFTEscrow dat NFTMarketEscrowFinal.Contract.Buy ctx === True

-- | Buy succeeds if payment > price (overpay is allowed)
prop_buy_success_overpay
  :: NFTMarketEscrowFinal.Contract.NFTEscrowDatum
  -> Positive Integer
  -> Property
prop_buy_success_overpay dat (Positive extra) =
  let
    scriptIn   = mkScriptInput dat
    sellerAddr = Address (PubKeyCredential (NFTMarketEscrowFinal.Contract.sellerPkh dat)) Nothing
    payAmt     = NFTMarketEscrowFinal.Contract.price dat + extra
    payOut     = mkAdaOut sellerAddr payAmt

    ctx = mkContext [scriptIn] [payOut] dat []
  in
    NFTMarketEscrowFinal.Contract.validateNFTEscrow dat NFTMarketEscrowFinal.Contract.Buy ctx === True

-- | Buy fails if payment < price
prop_buy_fail_underpay
  :: NFTMarketEscrowFinal.Contract.NFTEscrowDatum
  -> Positive Integer
  -> Property
prop_buy_fail_underpay dat (Positive under) =
  under < NFTMarketEscrowFinal.Contract.price dat ==>
    let
      scriptIn   = mkScriptInput dat
      sellerAddr = Address (PubKeyCredential (NFTMarketEscrowFinal.Contract.sellerPkh dat)) Nothing
      payAmt     = NFTMarketEscrowFinal.Contract.price dat - under
      payOut     = mkAdaOut sellerAddr payAmt

      ctx = mkContext [scriptIn] [payOut] dat []
    in
      NFTMarketEscrowFinal.Contract.validateNFTEscrow dat NFTMarketEscrowFinal.Contract.Buy ctx === False

-- | Buy fails if the script input does NOT contain the required NFT
prop_buy_fail_no_nft
  :: NFTMarketEscrowFinal.Contract.NFTEscrowDatum
  -> Property
prop_buy_fail_no_nft dat =
  let
    nonNFTIn   = mkNonNFTInput
    sellerAddr = Address (PubKeyCredential (NFTMarketEscrowFinal.Contract.sellerPkh dat)) Nothing
    payOut     = mkAdaOut sellerAddr (NFTMarketEscrowFinal.Contract.price dat)

    ctx = mkContext [nonNFTIn] [payOut] dat []
  in
    NFTMarketEscrowFinal.Contract.validateNFTEscrow dat NFTMarketEscrowFinal.Contract.Buy ctx === False

-- | Combined failure: no NFT in input AND underpaying (all checks fail together)
prop_buy_fail_all_conditions
  :: NFTMarketEscrowFinal.Contract.NFTEscrowDatum
  -> Positive Integer
  -> Property
prop_buy_fail_all_conditions dat (Positive under) =
  under < NFTMarketEscrowFinal.Contract.price dat ==>
    let
      nonNFTIn   = mkNonNFTInput
      sellerAddr = Address (PubKeyCredential (NFTMarketEscrowFinal.Contract.sellerPkh dat)) Nothing
      payAmt     = NFTMarketEscrowFinal.Contract.price dat - under
      payOut     = mkAdaOut sellerAddr payAmt

      ctx = mkContext [nonNFTIn] [payOut] dat []
    in
      NFTMarketEscrowFinal.Contract.validateNFTEscrow dat NFTMarketEscrowFinal.Contract.Buy ctx === False

--------------------------------------------------------------------------------
-- Main: assemble all Buy tests into a TestTree

main :: IO ()
main =
  defaultMain $ testGroup "NFTMarketEscrowFinal – Buy Tests"
    [ testProperty "Buy succeeds with exact payment and NFT input"        prop_buy_success_exact
    , testProperty "Buy succeeds when overpaying"                        prop_buy_success_overpay
    , testProperty "Buy fails if underpaying"                            prop_buy_fail_underpay
    , testProperty "Buy fails if no NFT in script input"                 prop_buy_fail_no_nft
    , testProperty "Buy fails if no NFT AND underpaying (all fail)"      prop_buy_fail_all_conditions
    ]
