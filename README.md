# Smart Contracts-luxechain
This is the repository where all the smart contracts reside for project Luxechain.

#CARDANO SMART CONTRACT
NFTMarketEscrowFinal:- Cardano Escrow Smart Contract for on-chain NFT transfer from seller's wallet to buyer's wallet.
#Overview:
The Escrow smart contract provides an automated, trustless mechanism for securely transferring NFTs and funds during a watch sale.
#Flow:
1. Seller locks the NFT and price info into the escrow contract.
2. Buyer sends ADA.
3. Smart contract validates:
  ADA amount.
  NFT match.
  Script contains the particular NFT.
4. If successful:
  NFT goes to buyer.
  ADA goes to seller.
5. If cancelled by seller:
  NFT is returned to seller's wallet.

Contract.hs file contains the escrow smart contract codes for validations.
Compiler.hs file contains the code for writing the compiled plutus file to provided destination folder.
scripts folder contains onchain bashs-script files to make operations on-chain using cardano cli.
OffchainLucidCode folder contains Javascript codes to make operations on cardano smart contract off-chain using lucid-cardano js library.
Test folder contains proper unit test files and quick-check files.

#MIDNIGHT SMART CONTRACT
MidnightNFTVerificationCompactSmartContract:- Midnight Smart Contract that stores the hashedUsername - NFT ID in map registry for verification purpose.
#Overview:-
Midnight is a privacy-focused blockchain that leverages zero-knowledge (ZK) technology to enable data protection while maintaining the blockchain's transparency benefits. At its core, Midnight uses ZK SNARK (Zero-Knowledge Succinct Non-Interactive Arguments of Knowledge), which allows one party to prove a statement's validity without revealing any additional information beyond the claim itself.
#Flow:
1. The seller initiates the process by calling the LockNFT function. This function takes the NFT ID and the seller’s username as inputs.
2. The smart contract generates and stores the hash value of the NFT ID and the hash of the seller’s username. This hash mapping is stored securely to establish ownership and list the NFT on the marketplace. The hash is sent to the NFT Marketplace, effectively listing the NFT.
3. A buyer browses the NFT Marketplace and selects an NFT they want to purchase.
4. The buyer calls the verifyNFTOwnership circuit on the Midnight smart contract. This circuit checks whether the seller is the legitimate owner of the NFT using the stored hash values (without knowing the seller’s name/identity).
5. The smart contract returns the result to the buyer—either Verified or Not Verified.
6. If the verification is successful, the transaction proceeds: 1) The buyer receives the NFT, 2)The seller receives the payment.
7. After the successful purchase, the UnlockNFT function is called. This removes the entry from the registry of the NFT Id and the seller’s name mapping from the midnight smart contract.

NFTVerification.compact file contains a smart contract written in compact language in midnight blockchain for the purpose of NFT ownership storing and verification.
Test folder contains proper unit testing for the smart contract.
