# Change Mod
# chmod +x ./NFTMarketEscrowFinal/newBuy.sh
# ./NFTMarketEscrowFinal/newBuy.sh

#!/usr/bin/env bash
set -euo pipefail

# ─── CONFIGURATION ────────────────────────────────────────────────────
NETWORK="--testnet-magic 1"
SOCKET="--socket-path /home/$USER/git/cardano-node/preprod/db/node.socket"

SCRIPT_PLUTUS="NFTMarketEscrowFinal/escrow.json"
SCRIPT_ADDR_FILE="NFTMarketEscrowFinal/script.addr"
REDEEMER_FILE="NFTMarketEscrowFinal/redeemer-buy.json"
PROTOCOL_PARAMS="NFTMarketEscrowFinal/protocol.json"

BUYER_SKEY="NFTMarketEscrowFinal/buyer.skey"
BUYER_VKEY="NFTMarketEscrowFinal/buyer.vkey"
BUYER_ADDR_FILE="NFTMarketEscrowFinal/buyer.addr"
SELLER_ADDR_FILE="NFTMarketEscrowFinal/seller.addr"

# ---- VALUES TO INSERT ------------------------------------------------
SCRIPT_ADDR=$(< $SCRIPT_ADDR_FILE)
BUYER_ADDR=$(< $BUYER_ADDR_FILE)
SELLER_ADDR=$(< $SELLER_ADDR_FILE)

SCRIPT_NFT_UTXO="960f4b4c52a77a757398a45089d10e196f7dc87d00663782387ad3967ba74c89#0"   # NFT locked UTxO
BUYER_ADA_UTXO="69a4efd491d80ab30982bc71ac402f206f86803ba93728a6557ccadedc846b65#0" # Buyer's ADA UTxO
POLICY_ID="9988311d2c14baa91c6ca4709d1325232d6beb32f0f09f384522ac31"
TOKEN_NAME_HEX="526f6c65785375626d6172696e65724853444b53363736383838"
PRICE=10000000                            # must match datum price

# ─── 1) Create redeemer file ──────────────────────────────────────────
cat > $REDEEMER_FILE <<EOF
{
  "constructor": 0,
  "fields": []
}
EOF
echo "→ Redeemer 'Buy' created."

# ─── 2) Calculate min lovelace with NFT ───────────────────────────────
MIN_UTXO_RAW=$(cardano-cli conway transaction calculate-min-required-utxo \
  --protocol-params-file $PROTOCOL_PARAMS \
  --tx-out "$BUYER_ADDR + 1 $POLICY_ID.$TOKEN_NAME_HEX")
MIN_LOVELACE=$(echo $MIN_UTXO_RAW | sed 's/^Coin //')
echo "→ Minimum lovelace with NFT: $MIN_LOVELACE"

# ─── 3) Find collateral ───────────────────────────────────────────────
COLLATERAL=$(cardano-cli conway query utxo \
  $NETWORK $SOCKET \
  --address $BUYER_ADDR \
  --out-file /dev/stdout \
  | jq -r 'to_entries 
      | map(select(.value.value.lovelace and ( .value.value | length == 1)))
      | .[0].key')
echo "→ Collateral UTxO: $COLLATERAL"

# Null or empty check
if [ -z "$COLLATERAL" ] || [ "$COLLATERAL" = "null" ]; then
  echo "❌ No valid collateral UTxO found (must be pure ADA only)."
  echo "💡 Tip: Send a small amount of ADA (e.g., 2 ADA) to yourself in a new transaction to create a clean collateral UTxO."
  exit 1
fi

# ─── 4) Slot TTL setup ────────────────────────────────────────────────
# TIP_JSON=$(cardano-cli query tip $NETWORK $SOCKET)
# SLOT_NOW=$(echo "$TIP_JSON" | jq -r .slot)
# SLOT_TTL=$((SLOT_NOW + 600)) # 10 minutes
# echo "→ Slot Now: $SLOT_NOW"
# echo "→ Slot TTL (10 min later): $SLOT_TTL"

# ─── 5) Build transaction ─────────────────────────────────────────────
cardano-cli conway transaction build \
  $NETWORK $SOCKET \
  --tx-in $SCRIPT_NFT_UTXO \
  --tx-in-script-file $SCRIPT_PLUTUS \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file $REDEEMER_FILE \
  --tx-in-collateral $COLLATERAL \
  --tx-in $BUYER_ADA_UTXO \
  --tx-out "$BUYER_ADDR + $MIN_LOVELACE + 1 $POLICY_ID.$TOKEN_NAME_HEX" \
  --tx-out "$SELLER_ADDR + $PRICE" \
  --required-signer-hash $(cardano-cli address key-hash --payment-verification-key-file $BUYER_VKEY) \
  --change-address $BUYER_ADDR \
  --out-file NFTMarketEscrowFinal/unlock.tx

echo "✅ Transaction built: NFTMarketEscrowFinal/unlock.tx"

# ─── 6) Sign & Submit ─────────────────────────────────────────────────
cardano-cli conway transaction sign \
  --tx-file NFTMarketEscrowFinal/unlock.tx \
  --signing-key-file $BUYER_SKEY \
  $NETWORK \
  --out-file NFTMarketEscrowFinal/buy.signed

cardano-cli conway transaction submit \
  --tx-file NFTMarketEscrowFinal/buy.signed \
  $NETWORK $SOCKET

echo "✅ Buy transaction submitted. NFT should now be with the buyer."
