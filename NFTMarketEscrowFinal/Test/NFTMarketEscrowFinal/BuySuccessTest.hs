{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

 module Main where

import Prelude                 (IO, String, ($), (<>),Bool(..),Integer,Maybe(..),mempty)
import Test.Tasty             (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit       (testCase, (@?=))
import Plutus.V1.Ledger.Scripts

-- Plutus imports
import Plutus.V2.Ledger.Api
  ( PubKeyHash(..)
  , BuiltinByteString
  , CurrencySymbol(..)
  , TokenName(..)
  , POSIXTime(..)
  , Value
  , ScriptContext(..)
  , TxInfo(..)
  , TxOut(..)
  , TxInInfo(..)
  , TxOutRef(..)
  , Validator
  , Address(..)
  , Credential(..)
  , OutputDatum(..)
  , ScriptPurpose(..)
  )
import Plutus.V1.Ledger.Value    (singleton)
import Plutus.V1.Ledger.Interval (contains, to, from)
import qualified PlutusTx.AssocMap as AssocMap

-- Your contract module (export validateNFTEscrow, NFTEscrowDatum, NFTEscrowAction)
import qualified NFTMarketEscrowFinal.Contract
  ( validateNFTEscrow
  , NFTEscrowDatum(..)
  , NFTEscrowAction(..)
  )

-- | Mock seller and buyer pub key hashes
sellerPkh :: PubKeyHash
sellerPkh = PubKeyHash "deadbeef"
-- buyerPkh  = PubKeyHash "cafebabe"


-- near the top, after your seller/buyer PKHs:
dummyValHash :: ValidatorHash
dummyValHash = ValidatorHash "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"

-- | The NFT currency and name
policyId    :: CurrencySymbol
policyId    = CurrencySymbol "abcd1234"
tokenName   :: TokenName
tokenName   = TokenName "MyNFT"

-- | Price and expiry
price       :: Integer
price       = 10_000_000    -- 10 ADA

-- | Dummy hash for hashed owner (not used in this test)
dummyHash :: BuiltinByteString
dummyHash = "dummyHash"

-- expiryTime  :: POSIXTime
-- expiryTime  = POSIXTime 1_000_000_000

-- | Build the on‐chain datum
datum :: NFTMarketEscrowFinal.Contract.NFTEscrowDatum
datum = NFTMarketEscrowFinal.Contract.NFTEscrowDatum
  { NFTMarketEscrowFinal.Contract.sellerPkh    = sellerPkh
  , NFTMarketEscrowFinal.Contract.policyId     = policyId
  , NFTMarketEscrowFinal.Contract.tokenName    = tokenName
  , NFTMarketEscrowFinal.Contract.price        = price
  , NFTMarketEscrowFinal.Contract.hashedOwner  = dummyHash
  }


-- | Redeemer for Buy action
redeemer :: NFTMarketEscrowFinal.Contract.NFTEscrowAction
redeemer = NFTMarketEscrowFinal.Contract.Buy

-- | A UTxO at the script containing the NFT
scriptInput :: TxInInfo
scriptInput = TxInInfo
  { txInInfoOutRef = TxOutRef "txhash" 0
  , txInInfoResolved = TxOut
      { txOutAddress = Address (ScriptCredential dummyValHash) Nothing
      , txOutValue   = singleton policyId tokenName 1
      , txOutDatum   = OutputDatumHash ""
      , txOutReferenceScript = Nothing  -- not used by validateNFTEscrow
      }
  }

-- | Outputs: buyer gets NFT, seller gets ADA
-- buyerOutput :: TxOut
-- buyerOutput = TxOut
--   { txOutAddress = Address (PubKeyCredential buyerPkh) Nothing
--   , txOutValue   = singleton policyId tokenName 1
--   , txOutDatum   = NoOutputDatum
--   , txOutReferenceScript = Nothing
--   }

sellerOutput :: TxOut
sellerOutput = TxOut
  { txOutAddress = Address (PubKeyCredential sellerPkh) Nothing
  , txOutValue   = singleton "" "" price -- lovelaceValueOf price
  , txOutDatum   = NoOutputDatum
  , txOutReferenceScript = Nothing
  }

-- | Valid time range: before expiry
-- validRange :: POSIXTime -> POSIXTime -> Bool
-- validRange (POSIXTime _) (POSIXTime to') =
--   contains (to expiryTime) (from to')

-- | Construct a minimal TxInfo to satisfy the Buy path
txInfo :: TxInfo
txInfo = TxInfo
  { txInfoInputs        = [scriptInput]
  , txInfoOutputs       = [sellerOutput]
  , txInfoReferenceInputs  = []
  , txInfoFee           = mempty
  , txInfoMint          = mempty
  , txInfoDCert         = []
  , txInfoWdrl          = AssocMap.empty
  , txInfoRedeemers        = AssocMap.empty
  , txInfoData          = AssocMap.empty
  , txInfoId            = "dummy"
  }

-- | The ScriptContext for spending that UTxO
ctx :: ScriptContext
ctx = ScriptContext
  { scriptContextTxInfo    = txInfo
  , scriptContextPurpose   = Spending (TxOutRef "txhash" 0)
  }

-- | The Buy test case
buyTest :: TestTree
buyTest = testCase "Buy succeeds under valid conditions" $
  NFTMarketEscrowFinal.Contract.validateNFTEscrow datum redeemer ctx @?= True

-- | Collect all tests
tests :: TestTree
tests = testGroup "NFT Escrow Validator Tests"
  [ buyTest
  ]

-- | Main entry
main :: IO ()
main = defaultMain tests
