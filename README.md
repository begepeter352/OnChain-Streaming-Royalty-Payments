# OnChain Streaming Royalty Payments

A transparent and decentralized system for managing music royalty payments on the Stacks blockchain. This smart contract enables direct payments from listeners to artists for each stream, with transparent accounting and automatic distribution.

## Features

- Register artists and their payment addresses
- Add songs with customizable per-stream pricing
- Process streaming payments with automatic royalty distribution
- Track streaming history and statistics
- Configurable platform fees
- Complete transparency for all stakeholders

## Contract Functions

### Administrative Functions

- `register-artist`: Register a new artist (admin only)
- `register-artist-with-principal`: Register an artist with a specific payment address (admin only)
- `add-song`: Add a new song to the platform (admin only)
- `set-platform-fee`: Set the platform fee percentage (admin only)
- `set-min-stream-price`: Set the minimum price per stream (admin only)
- `deactivate-song`: Deactivate a song (admin only)
- `deactivate-artist`: Deactivate an artist (admin only)
- `transfer-ownership`: Transfer contract ownership (admin only)

### User Functions

- `stream-song`: Stream a song and pay the artist

### Read-Only Functions

- `get-artist`: Get artist information
- `get-song`: Get song information
- `get-listener`: Get listener statistics
- `get-stream`: Get stream details
- `get-owner`: Get contract owner
- `get-platform-fee`: Get current platform fee percentage
- `get-min-stream-price`: Get minimum stream price

## Usage Example

1. Register an artist:
```clarity
(contract-call? .royalty-stream register-artist "Artist Name")
```

2. Add a song:
```clarity
(contract-call? .royalty-stream add-song "Song Title" u1 u1000)
```

3. Stream a song:
```clarity
(contract-call? .royalty-stream stream-song u1)
```

## Implementation Details

- Artists receive payments directly to their registered principal address
- Platform fee is configurable (default: 5%)
- All streaming activity is recorded on-chain for transparency
- Minimum stream price is configurable (default: 1000 microSTX)
```
