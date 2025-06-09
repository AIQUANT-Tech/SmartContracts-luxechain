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

import {
  type CircuitContext,
  QueryContext,
  sampleContractAddress,
  constructorContext
} from "@midnight-ntwrk/compact-runtime";
import {
  Contract,
  type Ledger,
  ledger
} from "../../managed/NFTVerification/contract/index.cjs";
import {
  createNFTVerificationPrivateState,
  type NFTVerificationPrivateState,
  witnesses
} from "../../witnesses.js";

// This simulator allows testing the NFTVerification contract functionality
export class NFTVerificationSimulator {
  readonly contract: Contract<NFTVerificationPrivateState>;
  circuitContext: CircuitContext<NFTVerificationPrivateState>;

  constructor() {
    this.contract = new Contract<NFTVerificationPrivateState>(witnesses);
    const initialPrivateState = createNFTVerificationPrivateState();
    
    // const initialPrivateState: NFTVerificationPrivateState = {
    //   sellerUsername: undefined,
    //   nftID: undefined
    // };
    const {
      currentPrivateState,
      currentContractState,
      currentZswapLocalState
    } = this.contract.initialState(constructorContext(initialPrivateState, "0".repeat(64)));
    this.circuitContext = {
      currentPrivateState,
      currentZswapLocalState,
      originalState: currentContractState,
      transactionContext: new QueryContext(
        currentContractState.data,
        sampleContractAddress()
      )
    };
  }

  public getLedger(): Ledger {
    return ledger(this.circuitContext.transactionContext.state);
  }

  public getPrivateState(): NFTVerificationPrivateState {
    return this.circuitContext.currentPrivateState;
  }

  public lockNFT(username: string, nftID: Uint8Array): Uint8Array {
    // Set up the witness functions to return our test values
    // this.circuitContext.currentPrivateState.getSellerUsername = () => username;
    // this.circuitContext.currentPrivateState.getNftID = () => nftID;
    this.circuitContext.currentPrivateState = createNFTVerificationPrivateState(
      username,
      nftID
    );
    // Execute the lockNFT circuit
    const result = this.contract.impureCircuits.lockNFT(this.circuitContext);

    // Update the current context to be the result of executing the circuit
    this.circuitContext = result.context;

    // Return the hashed username from the result
    return result.result;
  }

  public verifyNFTOwnership(
    nftID: Uint8Array,
    hashedUsername: Uint8Array
  ): boolean {
    // Execute the verifyNFTOwnership circuit with the correct arguments
    const result = this.contract.impureCircuits.verifyNFTOwnership(
      this.circuitContext,
      nftID,
      hashedUsername
    );

    // Update the current context to be the result of executing the circuit
    this.circuitContext = result.context;

    // Return the verification result
    return result.result;
  }

  public unlockNFT(
    nftID: Uint8Array,
    hashedUsername: Uint8Array
  ): boolean {
    // Execute the unlockNFT circuit with the provided arguments
    const result = this.contract.impureCircuits.unlockNFT(
      this.circuitContext,
      nftID,
      hashedUsername
    );
  
    // Update the current context to be the result of executing the circuit
    this.circuitContext = result.context;
  
    // Return the result of the unlock operation
    return result.result;
  }
}

