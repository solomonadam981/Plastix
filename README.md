# Plastix - Plastic Credit System

A blockchain-based incentive system that rewards users with PLX tokens for verified plastic waste collection.

## Overview

Plastix enables users to earn cryptocurrency rewards by collecting and reporting plastic waste. The system requires verification of collections by authorized administrators before issuing token rewards.

## Features

- **Token System**: PLX tokens with 6 decimal places
- **Collection Tracking**: Record plastic waste collections with weight and location
- **Verification Process**: Admin verification required for reward distribution
- **User Profiles**: Track collection history and earned rewards
- **Location Analytics**: Monitor collection statistics by geographic location
- **Bulk Operations**: Process multiple verifications efficiently

## Contract Functions

### User Functions

#### `register-collector`
Register as a plastic waste collector.
```clarity
(contract-call? .Plastix register-collector)
```

#### `submit-collection`
Submit a plastic collection record for verification.
```clarity
(contract-call? .Plastix submit-collection u500 "Downtown Beach")
```
Parameters:
- `weight`: Weight in grams (minimum 100g)
- `location`: Collection location (max 64 characters)

#### `transfer`
Transfer PLX tokens to another user.
```clarity
(contract-call? .Plastix transfer u1000000 'SP1234...)
```

### Admin Functions

#### `verify-collection`
Verify a submitted collection and distribute rewards.
```clarity
(contract-call? .Plastix verify-collection u1)
```

#### `bulk-verify-collections`
Verify multiple collections at once.
```clarity
(contract-call? .Plastix bulk-verify-collections (list u1 u2 u3))
```

#### `set-min-collection-weight`
Update minimum collection weight requirement.
```clarity
(contract-call? .Plastix set-min-collection-weight u150)
```

#### `set-base-reward-rate`
Update reward rate (tokens per gram).
```clarity
(contract-call? .Plastix set-base-reward-rate u15)
```

### Read-Only Functions

#### `get-balance`
Check token balance for an address.
```clarity
(contract-call? .Plastix get-balance 'SP1234...)
```

#### `get-user-profile`
Get user's collection statistics.
```clarity
(contract-call? .Plastix get-user-profile 'SP1234...)
```

#### `get-collection-record`
Get details of a specific collection.
```clarity
(contract-call? .Plastix get-collection-record u1)
```

#### `get-location-stats`
Get collection statistics for a location.
```clarity
(contract-call? .Plastix get-location-stats "Downtown Beach")
```

#### `calculate-reward`
Calculate reward amount for a given weight.
```clarity
(contract-call? .Plastix calculate-reward u500)
```

#### `get-contract-stats`
Get overall contract statistics.
```clarity
(contract-call? .Plastix get-contract-stats)
```

## Workflow

1. **Registration**: Users call `register-collector` to create their profile
2. **Collection**: Users collect plastic waste and call `submit-collection` with weight and location
3. **Verification**: Contract owner verifies collections using `verify-collection`
4. **Rewards**: Verified collections automatically mint and distribute PLX tokens
5. **Usage**: Users can transfer tokens or accumulate for future incentives

## Token Economics

- **Symbol**: PLX
- **Decimals**: 6
- **Default Reward Rate**: 10 PLX per gram
- **Minimum Collection**: 100 grams
- **Verification Required**: Yes

## Deployment

1. Deploy the contract using Clarinet
2. Set appropriate reward rates using admin functions
3. Begin accepting collector registrations
4. Monitor and verify submitted collections

## Error Codes

- `u100`: Owner-only function
- `u101`: Record not found
- `u102`: Record already exists
- `u103`: Insufficient token balance
- `u104`: Invalid amount
- `u105`: User not verified
- `u106`: Collection not found
- `u107`: Collection already verified
- `u108`: Invalid location
