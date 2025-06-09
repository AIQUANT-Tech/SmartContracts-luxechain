import dotenv from "dotenv";
import { Lucid, Blockfrost, Constr, Data } from "lucid-cardano";
import { bech32 } from "bech32";
import fs from "fs";

dotenv.config();

async function main() {
  try {
    // ── 1. Load env variables ───────────────────────────────────────
    const {
      BLOCKFROST_URL,
      BLOCKFROST_API_KEY,
      NETWORK,
      SELLER_SKEY,
      SCRIPT_CBORHEX,
    } = process.env;

    if (
      !BLOCKFROST_URL ||
      !BLOCKFROST_API_KEY ||
      !SELLER_SKEY ||
      !SCRIPT_CBORHEX
    ) {
      throw new Error("Missing env variables");
    }

    // ── 2. Init Lucid and seller wallet ─────────────────────────────
    const lucid = await Lucid.new(
      new Blockfrost(BLOCKFROST_URL, BLOCKFROST_API_KEY),
      NETWORK
    );

    const skeyData = JSON.parse(fs.readFileSync(SELLER_SKEY, "utf8"));
    const rawKeyHex = skeyData.cborHex.slice(4);
    const rawKeyBytes = Buffer.from(rawKeyHex, "hex");
    const words = bech32.toWords(rawKeyBytes);
    const bech32Key = bech32.encode("ed25519_sk", words);

    lucid.selectWalletFromPrivateKey(bech32Key);
    const sellerAddr = await lucid.wallet.address();
    const sellerPKH =
      lucid.utils.getAddressDetails(sellerAddr).paymentCredential.hash;

    // ── 3. Constants ────────────────────────────────────────────────
    const escrowScript = {
      type: "PlutusV2",
      script: SCRIPT_CBORHEX,
    };

    const escrowAddr = lucid.utils.validatorToAddress(escrowScript);

    const POLICY_ID =
      "aba88cd67c314f573e55419f4bbb91ad2462549f6342069a851143ed";
    const TOKEN_NAME = "526f6c65785375626d6172696e65724853444b53363736383838";
    const ASSET_ID = POLICY_ID + TOKEN_NAME;

    const utxos = await lucid.utxosAt(escrowAddr);
    const utxo = utxos.find((utxo) => utxo.assets[ASSET_ID] === 1n);
    if (!utxo) throw new Error("NFT UTxO not found at script address");

    console.log("Found NFT UTxO to cancel:", utxo.txHash, utxo.outputIndex);

    // ── 4. Build Redeemer for Cancel ────────────────────────────────
    const redeemer = Data.to(new Constr(1, [])); // index 1 = Cancel

    // ── 5. Build the transaction ────────────────────────────────────
    const tx = await lucid
      .newTx()
      .collectFrom([utxo], redeemer)
      .payToAddress(sellerAddr, utxo.assets)
      .attachSpendingValidator(escrowScript)
      .addSignerKey(sellerPKH)
      .complete();

    // ── 6. Sign and submit ──────────────────────────────────────────
    const signedTx = await tx.sign().complete();
    const txHash = await signedTx.submit();

    console.log("Cancel transaction submitted! TxHash:", txHash);
    console.log("→ NFT refunded back to seller");

    process.exit(0);
  } catch (err) {
    console.error("Error in unlock-cancel.js:", err);
    process.exit(1);
  }
}

main();
