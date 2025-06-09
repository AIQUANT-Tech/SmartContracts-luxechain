# Change Mod
# chmod +x ./NFTMarketEscrowFinal/scripts/lock.sh
# ./src/NFTMarketEscrowFinal/scripts/lock.sh
 
#!/usr/bin/env bash
set -euo pipefail
 
# ─── CONFIGURATION ────────────────────────────────────────────────────
NETWORK="--testnet-magic 1"
SOCKET="--socket-path /home/$USER/git/cardano-node/preprod/db/node.socket"
 
SCRIPT_PLUTUS="NFTMarketEscrowFinal/escrow.json"
SCRIPT_ADDR_FILE="NFTMarketEscrowFinal/script.addr"
DATUM_FILE="NFTMarketEscrowFinal/datum.json"
PROTOCOL_PARAMS="NFTMarketEscrowFinal/protocol.json"
 
SELLER_VKEY="NFTMarketEscrowFinal/seller.vkey"
SELLER_SKEY="NFTMarketEscrowFinal/seller.skey"
SELLER_ADDR_FILE="NFTMarketEscrowFinal/seller.addr"
BUYER_VKEY="NFTMarketEscrowFinal/buyer.vkey"
 
# Buyer / Seller PKHs for datum:
SELLER_PKH="$(cardano-cli address key-hash --payment-verification-key-file $SELLER_VKEY)"
# BUYER_PKH="$(cardano-cli address key-hash --payment-verification-key-file $BUYER_VKEY)"
 
# ---- VALUES TO INSERT ------------------------------------------------
# UTxOs to spend:
SELLER_NFT_UTXO="3e009916c0f9a1389500a838f79032279907384b027171098d86c5a2641dec0d#0"
SELLER_ADA_UTXO="3e009916c0f9a1389500a838f79032279907384b027171098d86c5a2641dec0d#2"
 
# NFT identifiers
POLICY_ID="9988311d2c14baa91c6ca4709d1325232d6beb32f0f09f384522ac31"
TOKEN_NAME_HEX="526f6c65785375626d6172696e65724853444b53363736383838"
PRICE_LOVELACE=10000000 # 10 ADA
# ----------------------------------------------------------------------
 
# ─── 1) Generate datum.json 


  
# NOW_S=$(date +%s)
# NOW_MS=$(( NOW_S * 1000 ))
# DAY_MS=$(( 86400 * 1000 ))
# EXPIRY_MS=$(( NOW_MS + DAY_MS ))
 
cat > $DATUM_FILE <<EOF
{
  "constructor": 0,
  "fields": [
    { "bytes": "${SELLER_PKH}" },
    { "bytes": "${POLICY_ID}" },
    { "bytes": "${TOKEN_NAME_HEX}" },
    { "int": ${PRICE_LOVELACE} }
  ]
}
EOF
echo "→ datum.json written"
 
# ─── 2) Derive script address (once) ─────────────────────────────────
cardano-cli address build \
  --payment-script-file $SCRIPT_PLUTUS \
  --out-file $SCRIPT_ADDR_FILE \
  $NETWORK
SCRIPT_ADDR=$(< $SCRIPT_ADDR_FILE)
echo "→ script address:  $SCRIPT_ADDR"
 
# ─── 3) Pick collateral UTxO (pure‐ADA) ──────────────────────────────
COLLATERAL=$(cardano-cli conway query utxo \
  $NETWORK $SOCKET \
  --address $(< $SELLER_ADDR_FILE) \
  --out-file /dev/stdout \
  | jq -r 'to_entries 
      | map(select(.value.value.lovelace and ( .value.value | length == 1)))
      | .[0].key')
echo "→ collateral UTxO: $COLLATERAL"
 
# ─── 4) Compute min‐ADA needed for script output ──────────────────────
MIN_UTXO_RAW=$(cardano-cli conway transaction calculate-min-required-utxo \
  --protocol-params-file $PROTOCOL_PARAMS \
  --tx-out "$SCRIPT_ADDR + 1 $POLICY_ID.$TOKEN_NAME_HEX" \
  --tx-out-inline-datum-file $DATUM_FILE)
# strip the leading “Coin ” so we only get the integer lovelace
MIN_LOVELACE=$(echo $MIN_UTXO_RAW | sed 's/^Coin //')
echo "→ min‐ADA UTxO:      $MIN_LOVELACE lovelace"
 
# ─── 5) Build lock transaction ────────────────────────────────────────
cardano-cli conway transaction build \
  $NETWORK $SOCKET \
  --tx-in $SELLER_NFT_UTXO \
  --tx-in $SELLER_ADA_UTXO \
  --tx-in-collateral $COLLATERAL \
  --required-signer-hash $SELLER_PKH \
  --tx-out "$SCRIPT_ADDR + $MIN_LOVELACE + 1 $POLICY_ID.$TOKEN_NAME_HEX" \
  --tx-out-inline-datum-file $DATUM_FILE \
  --change-address $(< $SELLER_ADDR_FILE) \
  --out-file NFTMarketEscrowFinal/lock.tx
 
# ─── 6) Sign & submit ─────────────────────────────────────────────────
cardano-cli conway transaction sign \
  --tx-file NFTMarketEscrowFinal/lock.tx \
  --signing-key-file $SELLER_SKEY \
  $NETWORK \
  --out-file NFTMarketEscrowFinal/lock.tx
 
cardano-cli conway transaction submit \
  --tx-file NFTMarketEscrowFinal/lock.tx \
  $NETWORK $SOCKET
 
echo "✅ NFT locked at $SCRIPT_ADDR"