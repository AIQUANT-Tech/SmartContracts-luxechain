import dotenv from "dotenv";
import { Lucid, Blockfrost, Constr, Data } from "lucid-cardano";
import { bech32 } from "bech32";
import fs from "fs";
import { log } from "console";

dotenv.config();

async function main() {
  try {
    // ── 1. Load environment variables ─────────────────────────────────
    const {
      BLOCKFROST_URL,
      BLOCKFROST_API_KEY,
      NETWORK,
      BUYER_SKEY,
      SCRIPT_CBORHEX,
      SELLER_SKEY,
    } = process.env;

    if (
      !BLOCKFROST_URL ||
      !BLOCKFROST_API_KEY ||
      !BUYER_SKEY ||
      !SCRIPT_CBORHEX ||
      !SELLER_SKEY
    ) {
      throw new Error("Missing env variables");
    }

    // ── 2. Init Lucid and wallet ──────────────────────────────────────
    const lucid = await Lucid.new(
      new Blockfrost(BLOCKFROST_URL, BLOCKFROST_API_KEY),
      NETWORK
    );

    const skeyData = JSON.parse(fs.readFileSync(BUYER_SKEY, "utf8"));
    const rawKeyHex = skeyData.cborHex.slice(4);
    const rawKeyBytes = Buffer.from(rawKeyHex, "hex");
    const words = bech32.toWords(rawKeyBytes);
    const bech32Key = bech32.encode("ed25519_sk", words);

    lucid.selectWalletFromPrivateKey(bech32Key);

    const buyerAddr = await lucid.wallet.address();
    const buyerPKH =
      lucid.utils.getAddressDetails(buyerAddr).paymentCredential.hash;

    // ---- SELLER ADDRESS ---------------------------------------------

    const sellerSkeyData = JSON.parse(fs.readFileSync(SELLER_SKEY, "utf8"));
    const sellerRawKeyHex = sellerSkeyData.cborHex.slice(4);
    const sellerRawKeyBytes = Buffer.from(sellerRawKeyHex, "hex");
    const sellerWords = bech32.toWords(sellerRawKeyBytes);
    const sellerBech32Key = bech32.encode("ed25519_sk", sellerWords);

    const sellerLucid = await Lucid.new(
      new Blockfrost(BLOCKFROST_URL, BLOCKFROST_API_KEY),
      NETWORK
    );

    sellerLucid.selectWalletFromPrivateKey(sellerBech32Key);
    const sellerAddr = await sellerLucid.wallet.address();
    console.log("Seller address: ", sellerAddr);
    console.log("Buyer address: ", buyerAddr);

    // ── 3. Define constants ───────────────────────────────────────────
    const escrowScript = {
      type: "PlutusV2",
      script: SCRIPT_CBORHEX,
    };
    const escrowAddr = lucid.utils.validatorToAddress(escrowScript);

    const POLICY_ID =
      "624a660e3b9e987167f259ee544687df3d3771e52bb421e582f9b554"; // hex
    const TOKEN_NAME = "526f6c65785375626d6172696e65724853444b53363736383838"; // hex
    const ASSET_ID = POLICY_ID + TOKEN_NAME; // <policyId><tokenName>
    // const PRICE = 10_000_000n;
    // const DATUM_HASH = "<datum_hash_if_not_inline>"; // or inline datum below

    const utxos = await lucid.utxosAt(escrowAddr);

    // Find correct UTxO holding the NFT
    const utxo = utxos.find((utxo) => utxo.assets[ASSET_ID] === 1n);
    if (!utxo) throw new Error("NFT UTxO not found at script address");

    console.log("UTxO found:", utxo.txHash, utxo.outputIndex);

    // ── 4. Build Redeemer & inline datum (if needed) ──────────────────
    const redeemer = Data.to(new Constr(0, [])); // Use correct Constr index + fields

    const datum = utxo.datum;
    const inlineDatum = Data.from(datum);
    const parsedDatum = inlineDatum.fields;
    const PRICE = parsedDatum[3]; // Price set in datum
    // const expiryTime = parsedDatum[5];
    // console.log("expiryTime", expiryTime);
    const utxoLovelace = utxo.assets.lovelace;
    console.log("utxoLovelace: ", utxoLovelace);
    console.log("PRICE: ", PRICE);

    // const slot = lucid.currentSlot();
    // const POSIXTime = lucid.utils.slotToUnixTime(slot);
    // console.log(POSIXTime);
    // console.log(Date.now());
    // console.log(Number(expiryTime));
    // console.log(BigInt(POSIXTime));
    // console.log(Number(BigInt(POSIXTime)));

    // ── 5. Build transaction ──────────────────────────────────────────
    const tx = await lucid
      .newTx()
      .collectFrom([utxo], redeemer)
      .payToAddress(buyerAddr, utxo.assets)
      .payToAddress(sellerAddr, { lovelace: PRICE + utxoLovelace })
      .attachSpendingValidator(escrowScript)
      .addSignerKey(buyerPKH) // txInfo.signatories includes buyer
      // // .validFrom(POSIXTime)
      // .validTo(Number(BigInt(POSIXTime) + 60000n))
      // .validTo(Date.now())
      // .validTo(Number(expiryTime - 600n))
      .complete();

    // ── 6. Sign and submit ────────────────────────────────────────────
    const signedTx = await tx.sign().complete();
    const txHash = await signedTx.submit();

    console.log("Transaction submitted! TxHash:", txHash);
    console.log("→ Verify that the NFT has moved to the buyer's wallet.");

    process.exit(0);
  } catch (err) {
    console.error("Error in unlock-buy.js:", err);
    process.exit(1);
  }
}

main();

// 06f8e1bbf56671e253e3caf212ddcfec4732ef51110f773c6f631d2c49a30ecc without 0n
// 4b34483520e3a8ec02343c2b55816e765ed990bbc254e4f023863d9052690f5d with 0n
// d779ade800323e324d30313cfe3a9ba32d0d20da4df60fd1cda4b81f78706343 with 1n
