import { describe, expect, it, beforeEach } from "vitest";
import { Cl } from "@stacks/transactions";

const accounts = simnet.getAccounts();
const deployer = accounts.get("deployer")!;
const wallet1 = accounts.get("wallet_1")!;

describe("Bitcoin Yield Vaults", () => {
  beforeEach(() => {
    // Reset state before each test
  });

  describe("Vault Creation", () => {
    it("should create a new vault with initial deposit", () => {
      const initialDeposit = 5000000; // 5 STX
      const enableAutoCompound = true;
      
      const { result } = simnet.callPublicFn(
        "yield-vault",
        "create-vault",
        [Cl.uint(initialDeposit), Cl.bool(enableAutoCompound)],
        wallet1
      );
      
      expect(result).toBeOk(Cl.bool(true));
    });

    it("should reject vault creation with insufficient deposit", () => {
      const smallDeposit = 500000; // 0.5 STX (below minimum)
      const enableAutoCompound = false;
      
      const { result } = simnet.callPublicFn(
        "yield-vault",
        "create-vault",
        [Cl.uint(smallDeposit), Cl.bool(enableAutoCompound)],
        wallet1
      );
      
      expect(result).toBeErr(Cl.uint(1005)); // ERR_MINIMUM_STAKE_NOT_MET
    });
  });

  describe("Yield Compounding Feature", () => {
    beforeEach(() => {
      // Create vault for testing
      simnet.callPublicFn(
        "yield-vault",
        "create-vault",
        [Cl.uint(5000000), Cl.bool(true)],
        wallet1
      );
    });

    it("should get vault information with compound status", () => {
      const { result } = simnet.callReadOnlyFn(
        "yield-vault",
        "get-vault-balance-with-yield",
        [Cl.principal(wallet1)],
        wallet1
      );
      
      expect(result).toBeSome();
    });

    it("should check compound availability", () => {
      const { result } = simnet.callReadOnlyFn(
        "yield-vault",
        "can-compound",
        [Cl.principal(wallet1)],
        wallet1
      );
      
      expect(result).toBeTuple();
    });

    it("should toggle auto-compound setting", () => {
      const { result } = simnet.callPublicFn(
        "yield-vault",
        "set-auto-compound",
        [Cl.bool(false)],
        wallet1
      );
      
      expect(result).toBeOk(Cl.bool(false));
    });
  });

  describe("Protocol Statistics", () => {
    it("should return protocol stats", () => {
      const { result } = simnet.callReadOnlyFn(
        "yield-vault",
        "get-protocol-stats",
        [],
        deployer
      );
      
      expect(result).toBeTuple();
    });
  });
});
