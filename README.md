# Fractional Ownership of Real Estate via NFTs
This smart contract enables fractional ownership of real estate properties through NFTs on the Stacks blockchain.

## 🌟 Features

- Create tokenized real estate properties
- Buy and sell property shares
- Distribute rental revenue among shareholders
- Transfer shares between users
- Claim revenue based on ownership percentage

## 📋 Contract Functions

### Property Management
- `register-property`: Create new tokenized property
- `deactivate-property`: Disable trading for a property
- `add-revenue`: Add rental revenue to property pool

### Share Operations
- `buy-shares`: Purchase shares of a property
- `transfer-shares`: Transfer shares to another user
- `claim-revenue`: Claim your share of property revenue

### Read-Only Functions
- `get-property-details`: View property information
- `get-shares`: Check share ownership
- `get-claimable-revenue`: View claimable revenue

## 🚀 Usage

1. Deploy the contract
2. Register properties as contract owner
3. Users can buy shares using STX
4. Property revenue is distributed proportionally
5. Shareholders can claim their revenue share

## 💡 Example

```clarity
;; Register a new property
(contract-call? .real-estate-nft register-property "Luxury Villa" "Miami Beach" u1000 u100)

;; Buy 10 shares
(contract-call? .real-estate-nft buy-shares u1 u10)

;; Claim revenue
(contract-call? .real-estate-nft claim-revenue u1)
```

## ⚠️ Requirements

- Clarinet
- Stacks Wallet
- STX tokens for transactions
```
