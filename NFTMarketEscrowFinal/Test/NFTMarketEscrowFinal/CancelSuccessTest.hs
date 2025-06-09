{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Prelude                 (IO, String, ($), (<>), Bool(..), Integer, Maybe(..),mempty)
import Test.Tasty              (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit        (testCase, (@?=))
import Plutus.V1.Ledger.Scripts

-- Plutus imports
import Plutus.V2.Ledger.Api
  ( PubKeyHash(..)
  , BuiltinByteString
  , CurrencySymbol(..)
  , TokenName(..)
  , POSIXTime(..)
  , ScriptContext(..)
  , TxInfo(..)
  , TxOut(..)
  , TxInInfo(..)
  , TxOutRef(..)
  , Address(..)
  , Credential(..)
  , OutputDatum(..)
  , ScriptPurpose(..)
  , ValidatorHash
  )
import Plutus.V1.Ledger.Value    (singleton)
import Plutus.V1.Ledger.Interval (to)
import qualified PlutusTx.AssocMap as AssocMap

-- Your contract module
import qualified NFTMarketEscrowFinal.Contract
  ( validateNFTEscrow
  , NFTEscrowDatum(..)
  , NFTEscrowAction(..)
  )

-- Mock seller and buyer
sellerPkh :: PubKeyHash
sellerPkh = PubKeyHash "deadbeef"

-- Dummy script address
dummyValHash :: ValidatorHash
dummyValHash = ValidatorHash "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"

-- NFT asset
policyId  :: CurrencySymbol
policyId  = CurrencySymbol "abcd1234"
tokenName :: TokenName
tokenName = TokenName "MyNFT"

-- Price and deadline
price      :: Integer
price      = 10_000_000

-- Dummy hash for hashed owner (not used in this test)
dummyHash :: BuiltinByteString
dummyHash = "dummyHash"

-- On-chain datum
datum :: NFTMarketEscrowFinal.Contract.NFTEscrowDatum
datum = NFTMarketEscrowFinal.Contract.NFTEscrowDatum
  { NFTMarketEscrowFinal.Contract.sellerPkh  = sellerPkh
  , NFTMarketEscrowFinal.Contract.policyId   = policyId
  , NFTMarketEscrowFinal.Contract.tokenName  = tokenName
  , NFTMarketEscrowFinal.Contract.price      = price
  , NFTMarketEscrowFinal.Contract.hashedOwner = dummyHash
  }

-- Redeemer for Cancel
redeemer :: NFTMarketEscrowFinal.Contract.NFTEscrowAction
redeemer = NFTMarketEscrowFinal.Contract.Cancel

-- Script UTxO containing the NFT
scriptInput :: TxInInfo
scriptInput = TxInInfo
  { txInInfoOutRef = TxOutRef "txhash" 0
  , txInInfoResolved = TxOut
      { txOutAddress         = Address (ScriptCredential dummyValHash) Nothing
      , txOutValue           = singleton policyId tokenName 1
      , txOutDatum           = OutputDatumHash ""
      , txOutReferenceScript = Nothing
      }
  }

-- Expected output: NFT returned to seller
sellerOutput :: TxOut
sellerOutput = TxOut
  { txOutAddress         = Address (PubKeyCredential sellerPkh) Nothing
  , txOutValue           = singleton policyId tokenName 1
  , txOutDatum           = NoOutputDatum
  , txOutReferenceScript = Nothing
  }

-- Build ScriptContext for Cancel
ctx :: ScriptContext
ctx = let
    tx = TxInfo
      { txInfoInputs          = [scriptInput]
      , txInfoReferenceInputs = []
      , txInfoOutputs         = [sellerOutput]
      , txInfoFee             = mempty
      , txInfoMint            = mempty
      , txInfoDCert           = []
      , txInfoWdrl            = AssocMap.empty
      , txInfoSignatories     = [sellerPkh]
      , txInfoRedeemers       = AssocMap.empty
      , txInfoData            = AssocMap.empty
      , txInfoId              = "dummy"
      }
  in ScriptContext tx (Spending (TxOutRef "txhash" 0))

-- Success test
cancelSuccessTest :: TestTree
cancelSuccessTest = testCase "Cancel succeeds under valid conditions" $
  NFTMarketEscrowFinal.Contract.validateNFTEscrow datum redeemer ctx @?= True

-- Main
main :: IO ()
main = defaultMain $ testGroup "Cancel Success Test"
  [ cancelSuccessTest ]
