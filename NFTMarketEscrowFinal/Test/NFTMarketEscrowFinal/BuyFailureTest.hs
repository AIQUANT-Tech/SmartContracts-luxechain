{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Prelude                 (IO, String, ($), (<>), Bool(..), Integer, Maybe(..), mempty)
import Test.Tasty              (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit        (testCase, (@?=))
import Plutus.V1.Ledger.Scripts

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
import Plutus.V1.Ledger.Value  (singleton)
import Plutus.V1.Ledger.Interval (to)
import qualified PlutusTx.AssocMap as AssocMap

import qualified NFTMarketEscrowFinal.Contract
  ( validateNFTEscrow
  , NFTEscrowDatum(..)
  , NFTEscrowAction(..)
  )

-- shared keys
sellerPkh, otherPkh :: PubKeyHash
sellerPkh = PubKeyHash "deadbeef"
-- buyerPkh  = PubKeyHash "cafebabe"
otherPkh  = PubKeyHash "abcdef01"

dummyValHash :: ValidatorHash
dummyValHash = ValidatorHash "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"

policyId  :: CurrencySymbol
policyId  = CurrencySymbol "abcd1234"
tokenName :: TokenName
tokenName = TokenName "MyNFT"

price      :: Integer
price      = 10_000_000

-- | Dummy hash for hashed owner (not used in this test)
dummyHash :: BuiltinByteString
dummyHash = "dummyHash"
-- expiryTime :: POSIXTime
-- expiryTime = POSIXTime 1_000_000_000

-- on‐chain datum
datum :: NFTMarketEscrowFinal.Contract.NFTEscrowDatum
datum = NFTMarketEscrowFinal.Contract.NFTEscrowDatum
  { NFTMarketEscrowFinal.Contract.sellerPkh  = sellerPkh
  , NFTMarketEscrowFinal.Contract.policyId   = policyId
  , NFTMarketEscrowFinal.Contract.tokenName  = tokenName
  , NFTMarketEscrowFinal.Contract.price      = price
  , NFTMarketEscrowFinal.Contract.hashedOwner  = dummyHash
  }

redeemerBuy :: NFTMarketEscrowFinal.Contract.NFTEscrowAction
redeemerBuy = NFTMarketEscrowFinal.Contract.Buy

-- helper to build a script‐UTxO containing one NFT
scriptInputWithNFT :: TxInInfo
scriptInputWithNFT = TxInInfo
  { txInInfoOutRef   = TxOutRef "txhash" 0
  , txInInfoResolved = TxOut
      { txOutAddress         = Address (ScriptCredential dummyValHash) Nothing
      , txOutValue           = singleton policyId tokenName 1
      , txOutDatum           = OutputDatumHash ""
      , txOutReferenceScript = Nothing
      }
  }

-- helper: script‐UTxO that has no NFT at all
scriptInputNoNFT :: TxInInfo
scriptInputNoNFT = scriptInputWithNFT
  { txInInfoResolved = (txInInfoResolved scriptInputWithNFT)
      { txOutValue = singleton "" "" 0  -- empty of our NFT
      }
  }

-- expected correct outputs
-- goodBuyerOutput :: TxOut
-- goodBuyerOutput = TxOut
--   { txOutAddress         = Address (PubKeyCredential buyerPkh) Nothing
--   , txOutValue           = singleton policyId tokenName 1
--   , txOutDatum           = NoOutputDatum
--   , txOutReferenceScript = Nothing
--   }

goodSellerOutput :: Integer -> TxOut
goodSellerOutput adaAmt = TxOut
  { txOutAddress         = Address (PubKeyCredential sellerPkh) Nothing
  , txOutValue           = singleton "" "" adaAmt
  , txOutDatum           = NoOutputDatum
  , txOutReferenceScript = Nothing
  }

-- build ScriptContext easily
mkCtx
  ::[TxInInfo]     -- inputs (script & possibly others)
  -> [TxOut]        -- outputs
  -> ScriptContext
mkCtx ins outs =
  let txInfo = TxInfo
        { txInfoInputs          = ins
        , txInfoReferenceInputs = []
        , txInfoOutputs         = outs
        , txInfoFee             = mempty
        , txInfoMint            = mempty
        , txInfoDCert           = []
        , txInfoWdrl            = AssocMap.empty
        , txInfoRedeemers       = AssocMap.empty
        , txInfoData            = AssocMap.empty
        , txInfoId              = "dummy"
        }
  in ScriptContext txInfo (Spending (TxOutRef "txhash" 0))

-- 1. wrong signer
-- fail_wrongSigner :: TestTree
-- fail_wrongSigner = testCase "Buy fails if not signed by buyer" $
--   NFTMarketEscrowFinal.Contract.validateNFTEscrow
--     datum
--     redeemerBuy
--     (mkCtx [otherPkh] expiryTime [scriptInputWithNFT] [ goodSellerOutput price])
--   @?= False

-- 2. expired
-- fail_expired :: TestTree
-- fail_expired = testCase "Buy fails after expiryTime" $
--   NFTMarketEscrowFinal.Contract.validateNFTEscrow
--     datum
--     redeemerBuy
--     (mkCtx [buyerPkh] (POSIXTime 1_000_000_001) [scriptInputWithNFT] [goodBuyerOutput, goodSellerOutput price])
--   @?= False

-- 3. script does not have NFT
fail_noNFT :: TestTree
fail_noNFT = testCase "Buy fails if script UTxO has no NFT" $
  NFTMarketEscrowFinal.Contract.validateNFTEscrow
    datum
    redeemerBuy
    (mkCtx  [scriptInputNoNFT] [ goodSellerOutput price])
  @?= False

-- 4. NFT not transferred to buyer
-- fail_missingNFT :: TestTree
-- fail_missingNFT = testCase "Buy fails if buyer output missing NFT" $
--   NFTMarketEscrowFinal.Contract.validateNFTEscrow
--     datum
--     redeemerBuy
--     (mkCtx [buyerPkh] expiryTime [scriptInputWithNFT] [goodSellerOutput price])
--   @?= False

-- 5. incorrect payment to seller
fail_badPayment :: TestTree
fail_badPayment = testCase "Buy fails if payment to seller is too small" $
  NFTMarketEscrowFinal.Contract.validateNFTEscrow
    datum
    redeemerBuy
    (mkCtx [scriptInputWithNFT] [ goodSellerOutput 5_000_000])
  @?= False

-- 6. all conditions fail
fail_allConditions :: TestTree
fail_allConditions = testCase "Buy fails when all failure conditions are present" $
  NFTMarketEscrowFinal.Contract.validateNFTEscrow
    datum
    redeemerBuy
    (mkCtx  [scriptInputNoNFT] [goodSellerOutput 5_000_000])
  @?= False

main :: IO ()
main = defaultMain $ testGroup "NFT Escrow Validator — Buy Failure Tests"
  [ fail_noNFT
  , fail_badPayment
  , fail_allConditions
  ]
