// lockNFT.js

import dotenv from "dotenv";
import { Lucid, Blockfrost, Constr, Data, fromText } from "lucid-cardano";
import { bech32 } from "bech32";

dotenv.config();

/**
 * Stand‑alone script to lock 1 NFT + 2 ADA into the
 * NFTMarketEscrow.Contract1 PlutusV2 script.
 *
 * Usage:
 *   node lockNftTest.js
 *
 * Make sure .env contains:
 *  • BLOCKFROST_URL
 *  • BLOCKFROST_PROJECT_ID
 *  • SELLER_SEED
 *  • SCRIPT_CBORHEX
 */

async function main() {
  try {
    // ── 1. Load & verify environment variables ─────────────────────────
    const BLOCKFROST_URL = process.env.BLOCKFROST_URL;
    const BLOCKFROST_PROJECT_ID = process.env.BLOCKFROST_API_KEY;
    const NETWORK = process.env.NETWORK;
    const SELLER_SKEY = process.env.SELLER_SKEY;
    const SCRIPT_CBORHEX = process.env.SCRIPT_CBORHEX;

    if (!BLOCKFROST_URL || !BLOCKFROST_PROJECT_ID) {
      throw new Error(
        "Please set BLOCKFROST_URL and BLOCKFROST_PROJECT_ID in .env"
      );
    }
    if (!SELLER_SKEY) {
      throw new Error("Please set SELLER_SKEY in .env");
    }
    if (!SCRIPT_CBORHEX) {
      throw new Error("Please set SCRIPT_CBORHEX in .env");
    }

    // ── 2. Initialize Lucid (Preprod) ────────────────────────────────────
    const lucid = await Lucid.new(
      new Blockfrost(BLOCKFROST_URL, BLOCKFROST_PROJECT_ID),
      NETWORK
    );

    const skeyData = JSON.parse(fs.readFileSync(SELLER_SKEY, "utf8"));
    //   console.log(skeyData);
    const rawKeyHex = skeyData.cborHex.slice(4);
    //   console.log(rawKeyHex);

    const rawKeyBytes = Buffer.from(rawKeyHex, "hex");
    const words = bech32.toWords(rawKeyBytes);
    const bech32Key = bech32.encode("ed25519_sk", words);

    lucid.selectWalletFromPrivateKey(bech32Key);

    console.log(await lucid.wallet.address());

    // ── 3. Hard‑coded NFT + sale details ─────────────────────────────────
    // You can change these values directly here if you prefer.
    // const BUYER_ADDR = fs
    //   .readFileSync("../NFTMarketEscrow/buyer.addr", "utf8")
    //   .trim();
    const POLICY_ID =
      "624a660e3b9e987167f259ee544687df3d3771e52bb421e582f9b554"; // hex
    const TOKEN_NAME = "526f6c65785375626d6172696e65724853444b53363736383838"; // hex
    const ASSET_ID = POLICY_ID + TOKEN_NAME; // "<policyId>.<tokenName>"
    const PRICE = 10_000_000; // 10 ADA in lovelace
    // const EXPIRY_TIME = 1_747_735_703_000; // future POSIX time in ms
    // const EXPIRY_TIME =
    //   lucid.utils.slotToUnixTime(lucid.currentSlot()) + 24 * 60 * 60 * 1000;

    // ── 4. Derive seller’s PubKeyHash from wallet ────────────────────────
    const sellerAddress = await lucid.wallet.address();
    const { paymentCredential } = lucid.utils.getAddressDetails(sellerAddress);
    const sellerPkh = paymentCredential?.hash;
    // const buyerPkh =
    //   lucid.utils.getAddressDetails(BUYER_ADDR).paymentCredential?.hash;
    if (!sellerPkh)
      throw new Error("Cannot derive seller’s PubKeyHash from skey");

    // if (!buyerPkh)
    //   throw new Error("Cannot derive buyer’s PubKeyHash from file");

    console.log("Seller PubKeyHash:", sellerPkh);
    // console.log("Buyer PubKeyHash:", buyerPkh);
    console.log("PolicyID:", POLICY_ID);
    console.log("TokenName:", TOKEN_NAME);
    console.log("AssetID:", ASSET_ID);
    console.log("Price (lovelace):", PRICE);
    // console.log("Expiry (ms):", EXPIRY_TIME);

    // ── 5. Build the on‑chain script & its address ────────────────────────
    const escrowScript = {
      type: "PlutusV2",
      script: SCRIPT_CBORHEX,
    };
    const escrowAddress = lucid.utils.validatorToAddress(escrowScript);
    console.log("Escrow script address:", escrowAddress);

    // ── 6. Build the inline NFTEscrowDatum ───────────────────────────────
    //    Constr 0 [ sellerPkh, buyerPkh, policyId, tokenName, price, expiryTime ]
    const nftDatum = new Constr(0, [
      sellerPkh, // PubKeyHash as ByteString
      // buyerPkh, // PubKeyHash as ByteString
      POLICY_ID, // CurrencySymbol as ByteString
      TOKEN_NAME, // TokenName as ByteString
      BigInt(PRICE), // Integer (lovelace)
      // BigInt(EXPIRY_TIME), // POSIXTime (ms)
    ]);
    // console.log("Inline datum constructed:", nftDatum);

    // ── 7. Create & submit the transaction ────────────────────────────────
    //    We lock exactly 1 NFT (ASSET_ID) + 2 ADA = 2_000_000 lovelace
    // const MIN_ADA = 2_000_000n;
    const tx = await lucid
      .newTx()
      .payToContract(
        escrowAddress,
        { inline: Data.to(nftDatum) },
        {
          // lovelace: MIN_ADA,
          [ASSET_ID]: 1n,
        }
      )
      .complete();

    console.log("Submitting transaction to lock NFT...");
    const signedTx = await tx.sign().complete();
    const txHash = await signedTx.submit();

    console.log("Transaction submitted! TxHash:", txHash);
    console.log(
      "→ Verify on a Preprod explorer that Approx 1.6 ADA + 1 NFT now sit at the script address."
    );

    process.exit(0);
  } catch (err) {
    console.error("Error in lock.js:", err);
    process.exit(1);
  }
}

main();
