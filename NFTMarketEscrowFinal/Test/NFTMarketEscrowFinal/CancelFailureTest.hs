  {-# LANGUAGE NumericUnderscores #-}
  {-# LANGUAGE OverloadedStrings #-}
  {-# LANGUAGE DataKinds #-}
  
  module Main where
  
  import Prelude                 (IO, String, ($), (<>), Bool(..), Integer, Maybe(..), mempty)
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
  
  import qualified NFTMarketEscrowFinal.Contract
    ( validateNFTEscrow
    , NFTEscrowDatum(..)
    , NFTEscrowAction(..)
    )
  
  -- Shared fixtures
  sellerPkh, otherPkh :: PubKeyHash
  sellerPkh = PubKeyHash "deadbeef"
  -- buyerPkh  = PubKeyHash "cafebabe"
  otherPkh  = PubKeyHash "01234567"    -- unauthorized
  
  dummyValHash :: ValidatorHash
  dummyValHash =
    ValidatorHash "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
  
  policyId  :: CurrencySymbol
  policyId  = CurrencySymbol "abcd1234"
  tokenName :: TokenName
  tokenName = TokenName "MyNFT"
  
  price      :: Integer
  price      = 10_000_000

  -- Dummy hash for hashed owner (not used in this test)
  dummyHash :: BuiltinByteString
  dummyHash = "dummyHash"
  -- expiryTime :: POSIXTime
  -- expiryTime = POSIXTime 1_000_000_000
  
  -- On‑chain datum
  datum :: NFTMarketEscrowFinal.Contract.NFTEscrowDatum
  datum = NFTMarketEscrowFinal.Contract.NFTEscrowDatum
    { NFTMarketEscrowFinal.Contract.sellerPkh  = sellerPkh
    , NFTMarketEscrowFinal.Contract.policyId   = policyId
    , NFTMarketEscrowFinal.Contract.tokenName  = tokenName
    , NFTMarketEscrowFinal.Contract.price      = price
    , NFTMarketEscrowFinal.Contract.hashedOwner  = dummyHash
    }
  
  redeemerCancel :: NFTMarketEscrowFinal.Contract.NFTEscrowAction
  redeemerCancel = NFTMarketEscrowFinal.Contract.Cancel
  
  -- A script UTxO containing the NFT
  scriptInput :: TxInInfo
  scriptInput = TxInInfo
    { txInInfoOutRef   = TxOutRef "txhash" 0
    , txInInfoResolved = TxOut
        { txOutAddress         = Address (ScriptCredential dummyValHash) Nothing
        , txOutValue           = singleton policyId tokenName 1
        , txOutDatum           = OutputDatumHash ""
        , txOutReferenceScript = Nothing
        }
    }
  
  -- A script‑UTxO that erroneously contains no NFT
  scriptInputNoNFT :: TxInInfo
  scriptInputNoNFT = scriptInput
    { txInInfoResolved = (txInInfoResolved scriptInput)
        { txOutValue = singleton "" "" 0  -- empty value
        }
    }
  
  -- The expected “return to seller” output
  sellerOutput :: TxOut
  sellerOutput = TxOut
    { txOutAddress         = Address (PubKeyCredential sellerPkh) Nothing
    , txOutValue           = singleton policyId tokenName 1
    , txOutDatum           = NoOutputDatum
    , txOutReferenceScript = Nothing
    }
  
  -- Helper to build a ScriptContext with custom inputs & outputs
  mkCtx
    :: [TxInInfo]   -- script inputs
    -> [PubKeyHash] -- signers   -- upper‐bound time
    -> [TxOut]      -- outputs
    -> ScriptContext
  mkCtx ins signers outs =
    let tx = TxInfo
          { txInfoInputs          = ins
          , txInfoReferenceInputs = []
          , txInfoOutputs         = outs
          , txInfoFee             = mempty
          , txInfoMint            = mempty
          , txInfoDCert           = []
          , txInfoWdrl            = AssocMap.empty
          , txInfoSignatories     = signers
          , txInfoRedeemers       = AssocMap.empty
          , txInfoData            = AssocMap.empty
          , txInfoId              = "dummy"
          }
    in ScriptContext tx (Spending (TxOutRef "txhash" 0))
  
  -- Failure 1: not signed by seller
  fail_wrongSigner :: TestTree
  fail_wrongSigner = testCase "Cancel fails if not signed by seller" $
    NFTMarketEscrowFinal.Contract.validateNFTEscrow
      datum
      redeemerCancel
      (mkCtx [scriptInput] [otherPkh] [sellerOutput])
    @?= False
  
  -- Failure 2: NFT not returned to seller (no outputs)
  fail_missingNFT :: TestTree
  fail_missingNFT = testCase "Cancel fails if NFT not returned" $
    NFTMarketEscrowFinal.Contract.validateNFTEscrow
      datum
      redeemerCancel
      (mkCtx [scriptInput] [sellerPkh] [])
    @?= False
  
  -- Failure 3: script UTxO lacks the NFT
  fail_noNFTInScript :: TestTree
  fail_noNFTInScript = testCase "Cancel fails if script UTxO has no NFT" $
    NFTMarketEscrowFinal.Contract.validateNFTEscrow
      datum
      redeemerCancel
      (mkCtx [scriptInputNoNFT] [sellerPkh] [sellerOutput])
    @?= False
  
  -- Failure 4: All conditions fail
  fail_allFailures :: TestTree
  fail_allFailures = testCase "Cancel fails if signed by wrong person, script has no NFT, and nothing returned" $
    NFTMarketEscrowFinal.Contract.validateNFTEscrow
      datum
      redeemerCancel
      (mkCtx [scriptInputNoNFT] [otherPkh] [])
    @?= False
  
  main :: IO ()
  main = defaultMain $ testGroup "NFT Escrow Validator — Cancel Failure Tests"
    [ fail_wrongSigner
    , fail_missingNFT
    , fail_noNFTInScript
    , fail_allFailures
    ]
  