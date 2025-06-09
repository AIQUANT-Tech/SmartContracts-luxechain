# Change Mod
# chmod +x ./NFTMarketEscrowFinal/scripts/cancel.sh
# ./NFTMarketEscrowFinal/scripts/cancel.sh

#!/usr/bin/env bash
set -euo pipefail

# ─── CONFIGURATION ────────────────────────────────────────────────────
NETWORK="--testnet-magic 1"
SOCKET="--socket-path /home/$USER/git/cardano-node/preprod/db/node.socket"

SCRIPT_PLUTUS="NFTMarketEscrowFinal/escrow.json"
SCRIPT_ADDR_FILE="NFTMarketEscrowFinal/script.addr"
REDEEMER_FILE="NFTMarketEscrowFinal/redeemer-cancel.json"
PROTOCOL_PARAMS="NFTMarketEscrowFinal/protocol.json"

SELLER_SKEY="NFTMarketEscrowFinal/seller.skey"
SELLER_VKEY="NFTMarketEscrowFinal/seller.vkey"
SELLER_ADDR_FILE="NFTMarketEscrowFinal/seller.addr"

# ---- VALUES TO INSERT ------------------------------------------------
SCRIPT_ADDR=$(< $SCRIPT_ADDR_FILE)
SELLER_ADDR=$(< $SELLER_ADDR_FILE)

SCRIPT_NFT_UTXO="e909437f47b5b189b51d76e2b339d2b9ecb1ee0eccc65c7f72eaf6d85c47533a#0"   # Locked NFT UTxO
SELLER_ADA_UTXO="e909437f47b5b189b51d76e2b339d2b9ecb1ee0eccc65c7f72eaf6d85c47533a#1"                                        # Any ADA UTxO for fee/change
POLICY_ID="9988311d2c14baa91c6ca4709d1325232d6beb32f0f09f384522ac31"
TOKEN_NAME_HEX="526f6c65785375626d6172696e65724853444b53363736383838"

# ─── 1) Create redeemer file ──────────────────────────────────────────
cat > $REDEEMER_FILE <<EOF
{
  "constructor": 1,
  "fields": []
}
EOF
echo "→ Redeemer 'Cancel' created."

# ─── 2) Calculate min lovelace ────────────────────────────────────────
MIN_UTXO_RAW=$(cardano-cli conway transaction calculate-min-required-utxo \
  --protocol-params-file $PROTOCOL_PARAMS \
  --tx-out "$SELLER_ADDR + 1 $POLICY_ID.$TOKEN_NAME_HEX")
MIN_LOVELACE=$(echo $MIN_UTXO_RAW | sed 's/^Coin //')
echo "→ Minimum lovelace with NFT: $MIN_LOVELACE"

# ─── 3) Find collateral ───────────────────────────────────────────────
COLLATERAL=$(cardano-cli conway query utxo \
  $NETWORK $SOCKET \
  --address $SELLER_ADDR \
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
  --tx-in $SELLER_ADA_UTXO \
  --tx-out "$SELLER_ADDR + $MIN_LOVELACE + 1 $POLICY_ID.$TOKEN_NAME_HEX" \
  --required-signer-hash $(cardano-cli address key-hash --payment-verification-key-file $SELLER_VKEY) \
  --change-address $SELLER_ADDR \
  --out-file NFTMarketEscrowFinal/refund.tx

echo "✅ Refund transaction built: NFTMarketEscrowFinal/refund.tx"

# ─── 6) Sign & Submit ─────────────────────────────────────────────────
cardano-cli conway transaction sign \
  --tx-file NFTMarketEscrowFinal/refund.tx \
  --signing-key-file $SELLER_SKEY \
  $NETWORK \
  --out-file NFTMarketEscrowFinal/cancel.signed

cardano-cli conway transaction submit \
  --tx-file NFTMarketEscrowFinal/cancel.signed \
  $NETWORK $SOCKET

echo "✅ Cancel transaction submitted. NFT refunded to seller."
