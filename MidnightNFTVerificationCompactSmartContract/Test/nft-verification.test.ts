// This file is part of midnightntwrk/example-nft-verification.
// Copyright (C) 2025 Midnight Foundation
// SPDX-License-Identifier: Apache-2.0
// Licensed under the Apache License, Version 2.0 (the "License");
// You may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

import { NFTVerificationSimulator } from "./nft-verification-simulator.js";
import {
  NetworkId,
  setNetworkId
} from "@midnight-ntwrk/midnight-js-network-id";
import { describe, it, expect } from "vitest";
// import { hexToBytes } from "@midnight-ntwrk/compact-runtime";
import { fromHex } from "@midnight-ntwrk/midnight-js-utils";

setNetworkId(NetworkId.Undeployed);

describe("NFT Verification smart contract", () => {
  // Sample test data
  const testNftID = fromHex(
    "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
  );
  const testUsername = "seller123";

  it("generates initial ledger state deterministically", () => {
    const simulator0 = new NFTVerificationSimulator();
    const simulator1 = new NFTVerificationSimulator();

    const ledger0 = simulator0.getLedger();
    const ledger1 = simulator1.getLedger();

    expect(ledger0.round).toEqual(ledger1.round);
    expect(ledger0.nftRegistry.size()).toEqual(ledger1.nftRegistry.size());
  });

  it("properly initializes ledger state and private state", () => {
    const simulator = new NFTVerificationSimulator();
    const initialLedgerState = simulator.getLedger();

    // Check that the round counter starts at 0
    expect(initialLedgerState.round).toEqual(0n);

    // Check that the NFT registry is initially empty
    expect(initialLedgerState.nftRegistry.size()).toEqual(0n);

    // Check that private state is initialized
    const initialPrivateState = simulator.getPrivateState();
    expect(initialPrivateState).toBeDefined();
  });

  it("locks an NFT and registers the seller", () => {
    const simulator = new NFTVerificationSimulator();

    // Lock the NFT
    const hashedUsername = simulator.lockNFT(testUsername, testNftID);

    // Verify the hashed username is returned
    expect(hashedUsername).toBeDefined();
    expect(hashedUsername.length).toBe(32);

    // Check that the NFT is registered in the registry
    const ledgerState = simulator.getLedger();
    expect(ledgerState.nftRegistry.member(testNftID)).toBe(true);

    // Check that the round counter was incremented
    expect(ledgerState.round).toEqual(1n);
  });

  it("verifies NFT ownership correctly", () => {
    const simulator = new NFTVerificationSimulator();

    // First lock the NFT
    const hashedUsername = simulator.lockNFT(testUsername, testNftID);

    // Then verify the ownership
    const isVerified = simulator.verifyNFTOwnership(testNftID, hashedUsername);

    // Check that verification succeeded
    expect(isVerified).toBe(true);
  });

  it("fails verification with incorrect hash", () => {
    const simulator = new NFTVerificationSimulator();

    // Lock the NFT
    simulator.lockNFT(testUsername, testNftID);

    // Create an incorrect hash
    const incorrectHash = fromHex(
      "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
    );

    // Verify with incorrect hash
    const isVerified = simulator.verifyNFTOwnership(testNftID, incorrectHash);

    // Check that verification failed
    expect(isVerified).toBe(false);
  });

  it("increments round counter correctly with multiple NFTs", () => {
    const simulator = new NFTVerificationSimulator();

    // Create multiple NFT IDs
    const nftID1 = fromHex(
      "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
    );
    const nftID2 = fromHex(
      "abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789"
    );

    // Lock first NFT
    simulator.lockNFT("seller1", nftID1);

    // Check round counter
    expect(simulator.getLedger().round).toEqual(1n);

    // Lock second NFT
    simulator.lockNFT("seller2", nftID2);

    // Check round counter incremented again
    expect(simulator.getLedger().round).toEqual(2n);
  });

  it("stores different hashes for the same username with different rounds", () => {
    const simulator = new NFTVerificationSimulator();

    // Create two different NFT IDs
    const nftID1 = fromHex(
      "1111111111111111111111111111111111111111111111111111111111111111"
    );
    const nftID2 = fromHex(
      "2222222222222222222222222222222222222222222222222222222222222222"
    );

    // Use the same username for both NFTs
    const username = "same_seller";

    // Lock first NFT
    const hash1 = simulator.lockNFT(username, nftID1);

    // Lock second NFT
    const hash2 = simulator.lockNFT(username, nftID2);

    // Hashes should be different due to different round numbers
    expect(hash1).not.toEqual(hash2);
  });

  it("allows multiple NFTs to be registered by the same seller", () => {
    const simulator = new NFTVerificationSimulator();

    // Create multiple NFT IDs
    const nftID1 = fromHex(
      "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
    );
    const nftID2 = fromHex(
      "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
    );

    // Use the same username
    const username = "collection_owner";

    // Lock both NFTs
    const hash1 = simulator.lockNFT(username, nftID1);
    const hash2 = simulator.lockNFT(username, nftID2);

    // Verify both NFTs are registered
    expect(simulator.getLedger().nftRegistry.member(nftID1)).toBe(true);
    expect(simulator.getLedger().nftRegistry.member(nftID2)).toBe(true);

    // Verify ownership of both NFTs
    expect(simulator.verifyNFTOwnership(nftID1, hash1)).toBe(true);
    expect(simulator.verifyNFTOwnership(nftID2, hash2)).toBe(true);
  });

  it("fails verification with incorrect NFT ID", () => {
    const simulator = new NFTVerificationSimulator();

    // Register an NFT
    const correctNftID = fromHex(
      "cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"
    );
    const incorrectNftID = fromHex(
      "dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd"
    );

    // Lock the correct NFT
    const hash = simulator.lockNFT("seller", correctNftID);

    // Try to verify with incorrect NFT ID
    // This should throw an error because the NFT isn't registered
    expect(() => {
      simulator.verifyNFTOwnership(incorrectNftID, hash);
    }).toThrow("NFT not registered in the system");
  });

  it("handles usernames of different lengths", () => {
    const simulator = new NFTVerificationSimulator();

    // Create NFT IDs
    const nftID1 = fromHex(
      "1212121212121212121212121212121212121212121212121212121212121212"
    );
    const nftID2 = fromHex(
      "3434343434343434343434343434343434343434343434343434343434343434"
    );

    // Use usernames of different lengths
    const shortUsername = "bob";
    const longUsername =
      "this_is_a_very_long_username_that_tests_the_hashing_function_with_more_characters";

    // Lock NFTs with different username lengths
    const hash1 = simulator.lockNFT(shortUsername, nftID1);
    const hash2 = simulator.lockNFT(longUsername, nftID2);

    // Verify both NFTs are registered correctly
    expect(simulator.verifyNFTOwnership(nftID1, hash1)).toBe(true);
    expect(simulator.verifyNFTOwnership(nftID2, hash2)).toBe(true);
  });

  it("registry size increases with each new NFT", () => {
    const simulator = new NFTVerificationSimulator();

    // Initial registry should be empty
    expect(simulator.getLedger().nftRegistry.size()).toEqual(0n);

    // Register first NFT
    simulator.lockNFT(
      "seller1",
      fromHex(
        "1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a"
      )
    );
    expect(simulator.getLedger().nftRegistry.size()).toEqual(1n);

    // Register second NFT
    simulator.lockNFT(
      "seller2",
      fromHex(
        "2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b"
      )
    );
    expect(simulator.getLedger().nftRegistry.size()).toEqual(2n);

    // Register third NFT
    simulator.lockNFT(
      "seller3",
      fromHex(
        "3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c"
      )
    );
    expect(simulator.getLedger().nftRegistry.size()).toEqual(3n);
  });
  it("unlocks an NFT after verification", () => {
    const simulator = new NFTVerificationSimulator();
  
    // First lock the NFT
    const hashedUsername = simulator.lockNFT(testUsername, testNftID);
  
    // Then unlock the NFT
    const unlockResult = simulator.unlockNFT(testNftID, hashedUsername);
  
    // Check that unlocking succeeded
    expect(unlockResult).toBe(true);
  
    // Verify the NFT is no longer in the registry
    expect(simulator.getLedger().nftRegistry.member(testNftID)).toBe(false);
  });
  
  it("fails to unlock NFT with incorrect hash", () => {
    const simulator = new NFTVerificationSimulator();
  
    // Lock the NFT
    simulator.lockNFT(testUsername, testNftID);
  
    // Create an incorrect hash
    const incorrectHash = fromHex(
      "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
    );
  
    // Try to unlock with incorrect hash
    expect(() => {
      simulator.unlockNFT(testNftID, incorrectHash);
    }).toThrow("Not authorized to unlock the NFT");
  });
  
  it("fails to unlock NFT that doesn't exist", () => {
    const simulator = new NFTVerificationSimulator();
    
    // Create an NFT ID that hasn't been registered
    const unregisteredNftID = fromHex(
      "9999999999999999999999999999999999999999999999999999999999999999"
    );
    
    // Create a hash
    const someHash = fromHex(
      "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
    );
    
    // Try to unlock an unregistered NFT
    expect(() => {
      simulator.unlockNFT(unregisteredNftID, someHash);
    }).toThrow("NFT not registered in the system");
  });
  
  it("completes a full lock-verify-unlock cycle", () => {
    const simulator = new NFTVerificationSimulator();
    
    // Initial registry should be empty
    expect(simulator.getLedger().nftRegistry.size()).toEqual(0n);
    
    // Lock the NFT
    const hashedUsername = simulator.lockNFT(testUsername, testNftID);
    
    // Registry should have one entry
    expect(simulator.getLedger().nftRegistry.size()).toEqual(1n);
    
    // Verify ownership
    const isVerified = simulator.verifyNFTOwnership(testNftID, hashedUsername);
    expect(isVerified).toBe(true);
    
    // Unlock the NFT
    const unlockResult = simulator.unlockNFT(testNftID, hashedUsername);
    expect(unlockResult).toBe(true);
    
    // Registry should be empty again
    expect(simulator.getLedger().nftRegistry.size()).toEqual(0n);
  });

  it("Two users can not lock same NFT", () => {
    const simulator = new NFTVerificationSimulator();
    
    // Initial registry should be empty
    expect(simulator.getLedger().nftRegistry.size()).toEqual(0n);
    
    // Lock the NFTq
    simulator.lockNFT("testUsername", testNftID);

    // Registry should have one entry
    expect(simulator.getLedger().nftRegistry.size()).toEqual(1n);
    
    //Different user wants to lock same NFT
    expect(() => {
      simulator.lockNFT("testUsername123", testNftID);
    }).toThrow("NFT is already registered in the system");
    
    // Registry should be empty again
    expect(simulator.getLedger().nftRegistry.size()).toEqual(1n);
  });
  it("maintains unique hashed usernames for different NFTs", () => {
    const simulator = new NFTVerificationSimulator();
    
    // Create two different NFT IDs
    const nftID1 = fromHex(
      "5555555555555555555555555555555555555555555555555555555555555555"
    );
    const nftID2 = fromHex(
      "6666666666666666666666666666666666666666666666666666666666666666"
    );
    
    // Use different usernames
    const username1 = "seller_one";
    const username2 = "seller_two";
    
    // Lock both NFTs
    const hash1 = simulator.lockNFT(username1, nftID1);
    const hash2 = simulator.lockNFT(username2, nftID2);
    
    // Verify hashes are different
    expect(hash1).not.toEqual(hash2);
    
    // Verify cross-verification fails (hash1 doesn't work for nftID2)
    expect(simulator.verifyNFTOwnership(nftID2, hash1)).toBe(false);
    expect(simulator.verifyNFTOwnership(nftID1, hash2)).toBe(false);
  });
  
  it("allows re-registering an NFT after it's been unlocked", () => {
    const simulator = new NFTVerificationSimulator();
    
    // First owner locks the NFT
    const hash1 = simulator.lockNFT("first_owner", testNftID);
    
    // First owner unlocks the NFT
    simulator.unlockNFT(testNftID, hash1);
    
    // Verify NFT is no longer in registry
    expect(simulator.getLedger().nftRegistry.member(testNftID)).toBe(false);
    
    // Second owner locks the same NFT
    const hash2 = simulator.lockNFT("second_owner", testNftID);
    
    // Verify NFT is now registered again
    expect(simulator.getLedger().nftRegistry.member(testNftID)).toBe(true);
    
    // Verify old hash no longer works
    expect(simulator.verifyNFTOwnership(testNftID, hash1)).toBe(false);
    
    // Verify new hash works
    expect(simulator.verifyNFTOwnership(testNftID, hash2)).toBe(true);
  });
  
  it("handles special characters in usernames", () => {
    const simulator = new NFTVerificationSimulator();
    
    const specialUsername = "user!@#$%^&*()_+{}|:<>?~";
    const nftID = fromHex(
      "7777777777777777777777777777777777777777777777777777777777777777"
    );
    
    // Lock NFT with username containing special characters
    const hash = simulator.lockNFT(specialUsername, nftID);
    
    // Verify NFT is registered
    expect(simulator.getLedger().nftRegistry.member(nftID)).toBe(true);
    
    // Verify ownership
    expect(simulator.verifyNFTOwnership(nftID, hash)).toBe(true);
  });
  
  it("maintains consistent hashing for the same inputs", () => {
    // Create two separate simulators
    const simulator1 = new NFTVerificationSimulator();
    const simulator2 = new NFTVerificationSimulator();
    
    // Use the same inputs in both simulators
    const username = "consistent_user";
    const nftID = fromHex(
      "8888888888888888888888888888888888888888888888888888888888888888"
    );
    
    // Lock NFT in both simulators
    const hash1 = simulator1.lockNFT(username, nftID);
    
    // Reset the second simulator's state to initial
    const simulator3 = new NFTVerificationSimulator();
    
    // Lock with same inputs
    const hash3 = simulator3.lockNFT(username, nftID);
    
    // Hashes should be identical for the same inputs
    expect(hash1).toEqual(hash3);
  });
  
  it("handles concurrent operations on multiple NFTs", () => {
    const simulator = new NFTVerificationSimulator();
    
    // Create multiple NFT IDs
    const nftIDs = [
      fromHex("a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1"),
      fromHex("b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2"),
      fromHex("c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3")
    ];
    
    // Lock all NFTs
    const hashes = nftIDs.map((id, index) => 
      simulator.lockNFT(`user_${index}`, id)
    );
    
    // Verify all NFTs are registered
    nftIDs.forEach(id => {
      expect(simulator.getLedger().nftRegistry.member(id)).toBe(true);
    });
    
    // Verify all ownerships
    nftIDs.forEach((id, index) => {
      expect(simulator.verifyNFTOwnership(id, hashes[index])).toBe(true);
    });
    
    // Unlock middle NFT
    simulator.unlockNFT(nftIDs[1], hashes[1]);
    
    // Verify middle NFT is removed but others remain
    expect(simulator.getLedger().nftRegistry.member(nftIDs[0])).toBe(true);
    expect(simulator.getLedger().nftRegistry.member(nftIDs[1])).toBe(false);
    expect(simulator.getLedger().nftRegistry.member(nftIDs[2])).toBe(true);
  });
});


