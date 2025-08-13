# 🏦 Bitcoin Yield Vaults

> Auto-compound STX/BTC staking strategies via Clarity smart contracts

## 🎯 Features

- Deposit STX tokens into yield-generating vaults
- Auto-compound rewards for maximized returns  
- Withdraw funds anytime
- Track rewards and positions
- Simple and secure staking mechanism

## 🚀 Getting Started

### Prerequisites

- [Clarinet](https://github.com/hirosystems/clarinet)
- [Stacks Wallet](https://www.hiro.so/wallet)

### Usage

1. Deploy the contract
2. Deposit STX (minimum 1M µSTX)
3. Wait for rewards to accumulate
4. Compound or claim rewards
5. Withdraw when ready

## 📋 Contract Functions

### Public Functions

- `deposit(amount: uint)` - Deposit STX tokens
- `withdraw(amount: uint)` - Withdraw STX tokens
- `claim-rewards()` - Claim accumulated rewards
- `compound-rewards()` - Auto-compound rewards back into deposit

### Read-Only Functions

- `get-user-deposit(user: principal)` - Get user's deposit amount
- `get-user-rewards(user: principal)` - Get user's unclaimed rewards
- `get-total-deposits()` - Get total deposits in vault

## 🔒 Security

- Locked state protection
- Minimum deposit requirements
- Balance checks
- Principal-based access control

## 📜 License

MIT
```

