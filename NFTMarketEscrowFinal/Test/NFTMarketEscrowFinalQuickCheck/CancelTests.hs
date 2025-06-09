{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NumericUnderscores #-}

module Main where

import Prelude
  ( String, IO, ($), (.), (<$>), return, Maybe(..), mempty, Bool(..), Eq(..)
  , Int, Integer, (>), head, (-), (+), (/=)
  )
import Test.Tasty                ( defaultMain, testGroup, TestTree )
import Test.Tasty.QuickCheck     ( testProperty, Property )
import Test.QuickCheck           ( Arbitrary(..), choose, vectorOf, Gen, forAll, (===), (==>) )

import PlutusTx.Builtins.Internal ( BuiltinByteString )
import Data.String                ( IsString(..) )

import Plutus.V1.Ledger.Interval  ( to )
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
    return $ NFTMarketEscrowFinal.Contract.NFTEscrowDatum
      { NFTMarketEscrowFinal.Contract.sellerPkh = seller
      , NFTMarketEscrowFinal.Contract.policyId  = policy
      , NFTMarketEscrowFinal.Contract.tokenName = name
      , NFTMarketEscrowFinal.Contract.price     = priceV
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
      , txInfoValidRange      = to 1000      -- no expiry check in new contract
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

-- Create an output returning exactly one NFT to given PubKeyHash
mkNFTReturn
  :: PubKeyHash
  -> CurrencySymbol
  -> TokenName
  -> Integer
  -> TxOut
mkNFTReturn pkh cs tn qty =
  TxOut
    (Address (PubKeyCredential pkh) Nothing)
    (singleton cs tn qty)
    NoOutputDatum
    Nothing

-- A UTxO with no NFT (for failure cases)
mkNonNFTInput :: TxOut
mkNonNFTInput =
  TxOut
    (Address (ScriptCredential dummyHash) Nothing)
    mempty
    (OutputDatumHash "")
    Nothing

--------------------------------------------------------------------------------
-- Properties for Cancel

-- | Cancel succeeds if:
--   1. script input contains the NFT
--   2. output returns that NFT to the seller
--   3. transaction is signed by seller
prop_cancel_success
  :: NFTMarketEscrowFinal.Contract.NFTEscrowDatum
  -> Property
prop_cancel_success dat =
  let
    -- script input has the NFT
    scriptIn   = mkScriptInput dat

    -- return NFT back to sellerPkh
    sellerPkh' = NFTMarketEscrowFinal.Contract.sellerPkh dat
    cs'        = NFTMarketEscrowFinal.Contract.policyId dat
    tn'        = NFTMarketEscrowFinal.Contract.tokenName dat
    returnOut  = mkNFTReturn sellerPkh' cs' tn' 1

    -- build context signed by seller
    ctx = mkContext [scriptIn] [returnOut] dat [sellerPkh']
  in
    NFTMarketEscrowFinal.Contract.validateNFTEscrow dat NFTMarketEscrowFinal.Contract.Cancel ctx === True

-- | Cancel fails if not signed by seller
prop_cancel_fail_not_signed
  :: NFTMarketEscrowFinal.Contract.NFTEscrowDatum
  -> Property
prop_cancel_fail_not_signed dat =
  let
    scriptIn   = mkScriptInput dat
    sellerPkh' = NFTMarketEscrowFinal.Contract.sellerPkh dat
    cs'        = NFTMarketEscrowFinal.Contract.policyId dat
    tn'        = NFTMarketEscrowFinal.Contract.tokenName dat
    returnOut  = mkNFTReturn sellerPkh' cs' tn' 1

    -- no signatories
    ctx = mkContext [scriptIn] [returnOut] dat []
  in
    NFTMarketEscrowFinal.Contract.validateNFTEscrow dat NFTMarketEscrowFinal.Contract.Cancel ctx === False

-- | Cancel fails if script input does NOT contain the NFT
prop_cancel_fail_no_nft_in_input
  :: NFTMarketEscrowFinal.Contract.NFTEscrowDatum
  -> Property
prop_cancel_fail_no_nft_in_input dat =
  let
    nonNFTIn   = mkNonNFTInput
    sellerPkh' = NFTMarketEscrowFinal.Contract.sellerPkh dat
    cs'        = NFTMarketEscrowFinal.Contract.policyId dat
    tn'        = NFTMarketEscrowFinal.Contract.tokenName dat
    returnOut  = mkNFTReturn sellerPkh' cs' tn' 1

    ctx = mkContext [nonNFTIn] [returnOut] dat [sellerPkh']
  in
    NFTMarketEscrowFinal.Contract.validateNFTEscrow dat NFTMarketEscrowFinal.Contract.Cancel ctx === False

-- | Cancel fails if NFT is not returned to seller (e.g., returned to someone else)
prop_cancel_fail_not_returned_to_seller
  :: NFTMarketEscrowFinal.Contract.NFTEscrowDatum
  -> PubKeyHash    -- ^ Some random other recipient
  -> Property
prop_cancel_fail_not_returned_to_seller dat otherPkh =
  otherPkh /= NFTMarketEscrowFinal.Contract.sellerPkh dat ==>
  let
    scriptIn   = mkScriptInput dat
    cs'        = NFTMarketEscrowFinal.Contract.policyId dat
    tn'        = NFTMarketEscrowFinal.Contract.tokenName dat

    -- return NFT to a wrong PubKeyHash
    wrongReturnOut = mkNFTReturn otherPkh cs' tn' 1

    ctx = mkContext [scriptIn] [wrongReturnOut] dat [NFTMarketEscrowFinal.Contract.sellerPkh dat]
  in
    NFTMarketEscrowFinal.Contract.validateNFTEscrow dat NFTMarketEscrowFinal.Contract.Cancel ctx === False

-- | Combined failure: no NFT in input AND not signed by seller
prop_cancel_fail_all_conditions
  :: NFTMarketEscrowFinal.Contract.NFTEscrowDatum
  -> PubKeyHash
  -> Property
prop_cancel_fail_all_conditions dat randomPkh =
  let
    nonNFTIn       = mkNonNFTInput
    cs'            = NFTMarketEscrowFinal.Contract.policyId dat
    tn'            = NFTMarketEscrowFinal.Contract.tokenName dat

    -- even if we try returning to seller, input has no NFT and signer is random
    returnOut      = mkNFTReturn (NFTMarketEscrowFinal.Contract.sellerPkh dat) cs' tn' 1

    ctx = mkContext [nonNFTIn] [returnOut] dat [randomPkh]
  in
    NFTMarketEscrowFinal.Contract.validateNFTEscrow dat NFTMarketEscrowFinal.Contract.Cancel ctx === False

--------------------------------------------------------------------------------
-- Main: assemble all Cancel tests into a TestTree

main :: IO ()
main =
  defaultMain $ testGroup "NFTMarketEscrowFinal – Cancel Tests"
    [ testProperty "Cancel succeeds when NFT is returned to seller and signed by seller"
        prop_cancel_success
    , testProperty "Cancel fails if not signed by seller"
        prop_cancel_fail_not_signed
    , testProperty "Cancel fails when no NFT in script input"
        prop_cancel_fail_no_nft_in_input
    , testProperty "Cancel fails if NFT returned to wrong recipient"
        prop_cancel_fail_not_returned_to_seller
    , testProperty "Cancel fails when no NFT and not signed by seller"
        prop_cancel_fail_all_conditions
    ]
