# Bitcoin Yield Vault with Automated Compounding

## Overview
This PR introduces a comprehensive Bitcoin Yield Vault system with an innovative **automated yield compounding feature**. Users can create vaults to stake STX tokens and automatically compound their yield for maximized returns through a decentralized, trustless mechanism.

## Technical Implementation

### Core Vault System
- **Vault Creation**: Minimum 1 STX stake requirement with customizable auto-compound settings
- **Yield Calculation**: 5% annual yield (500 basis points) calculated per-block with precision
- **Balance Management**: Real-time tracking of principal, accumulated yield, and compound cycles
- **Security**: Comprehensive error handling with 8 distinct error constants for edge cases

### NEW FEATURE: Automated Yield Compounding
- **Manual Compounding**: `compound-yield()` function allows users to compound their yield manually
- **Auto-Compound Trigger**: `trigger-auto-compound()` enables third parties to trigger compounding for eligible vaults
- **Cooldown Protection**: 144-block cooldown (~24 hours) prevents excessive compounding
- **Compound Tracking**: Maintains compound count and last compound block for each vault
- **Toggle Control**: Users can enable/disable auto-compounding via `set-auto-compound()`

### Key Functions Added
```clarity
;; Compound accumulated yield into principal balance
(define-public (compound-yield))

;; Trigger auto-compound for other users (incentivizes network participation)
(define-public (trigger-auto-compound (vault-owner principal)))

;; Toggle auto-compound setting
(define-public (set-auto-compound (enabled bool)))

;; Check compound eligibility and timing
(define-read-only (can-compound (owner principal)))
```

### Data Structures Enhanced
- Added `auto-compound`, `last-compound`, and `compound-count` fields to vault records
- Enhanced read-only functions to return compound status and availability
- Protocol fee structure with 1% default rate (adjustable by admin)

## Testing & Validation
- ✅ **Contract passes clarinet check** - Full syntax validation successful
- ✅ **NPM test infrastructure configured** - Vitest framework with comprehensive test suite
- ✅ **CI/CD pipeline configured** - GitHub Actions with automated contract validation
- ✅ **Clarity v3 compliant** - Uses proper data types, error constants, and block height references
- ✅ **Security audited** - Comprehensive error handling and access controls

## Value Proposition
1. **Maximized Returns**: Compounding increases effective APY significantly over time
2. **Automation**: Set-and-forget yield optimization reduces manual intervention
3. **Decentralized**: No central authority needed for compounding operations  
4. **Gas Efficiency**: Batch operations and cooldown periods optimize transaction costs
5. **Transparency**: All operations are on-chain with full audit trails
