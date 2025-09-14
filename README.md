# db-server

A ReST-ish server and CLI tool to expose Cardano-node's DB.

![CI Build](https://github.com/pragma-org/db-server/actions/workflows/haskell.yml/badge.svg?branch=main)

# Build

### Prerequisites

* Install Haskell toolchain, for example using [GHCUp](https://www.haskell.org/ghcup/) or Nix
* Install Cardano node's system dependencies (see [cardano-node wiki](https://github.com/input-output-hk/cardano-node-wiki/wiki/install)

### Building

`db-server` affords using  [Shake](https://shakebuild.com) as a driver to build itself. This has the advantage of taking care of building and installing cardano-node's native dependencies.

```
./build.hs
```

### Testing

```
cabal test
```

# Usage

`db-server` can be used in two modes: as an HTTP server exposing some endpoints over an existing cardano-node DB, or as command-line query tool.

## Server mode

### Starting the server

To run `db-server` over a `preprod` network database:

* Follow instructions from [Mithril website](https://mithril.network/doc/manual/getting-started/bootstrap-cardano-node) to retrieve a preprod database using [_Preprod_ configuration](https://mithril.network/doc/manual/getting-started/network-configurations). If all goes well, you should have a directory containing a primed database for example in `./db`
* Retrieve all configuration files for `Preprod` network from the [environment](https://book.world.dev.cardano.org/env-preprod.html) page into a directory, say `./config`

Then from the source directory of `db-server`, starts the server with the following command:

```
cabal run db-server -- serve --db db --config config/config.json
```

In the terminal, one should start seeing JSON-formatted log entries like:

```
{"log":"TraceOpenEvent StartedOpeningDB","tag":"ChainDB","timestamp":"2025-01-17T21:35:25.84556Z"}
{"log":"TraceOpenEvent StartedOpeningImmutableDB","tag":"ChainDB","timestamp":"2025-01-17T21:35:25.845566Z"}
{"log":"TraceImmutableDBEvent (ChunkValidationEvent (StartedValidatingChunk 3735 3735))","tag":"ChainDB","timestamp":"2025-01-17T21:35:25.89414Z"}
...
{"log":{"host":"127.0.0.1","port":9003,"tag":"HttpServerListening"},"tag":"HttpServer","timestamp":"2025-01-17T21:35:38.118951Z"}
```

The last entry above with the `HttpServerListening` signals the database is open and the server can answer queries.

### Using the server

#### Retrieve header bytes

* `GET /blocks/:slot/:hash/header`: Retrieves raw hex-encoded bytes of a block header at given `:slot` and with given `:hash` (aka. _point_)
  * `200` : Returns the raw hex-encoded bytes for header
  * `400` : Wrongly formatted `:slot` or `:hash` paths
  * `404` : No block exists at given point

**Example** (result truncated for legibility):

```
% curl -v http://localhost:9003/blocks/69206375/6f99b5f3deaeae8dc43fce3db2f3cd36ad8ed174ca3400b5b1bed76fdf248912/header
828a1a0028375b1a0420016758209694aa14a063868d37c5601b7b...
```

#### Retrieve parent's header

* `GET /blocks/:slot/:hash/parent`: Retrieves raw hex-encoded bytes of the block header which is the parent of the header at given `:slot` and with given `:hash` (aka. _point_)
  * `200` : Returns the raw hex-encoded bytes for header
  * `400` : Wrongly formatted `:slot` or `:hash` paths
  * `404` : No block exists at given point

**Example** (result truncated for legibility):

```
% curl -v http://localhost:9003/blocks/69206375/6f99b5f3deaeae8dc43fce3db2f3cd36ad8ed174ca3400b5b1bed76fdf248912/parent
828a1a0028375b1a0420016758209694aa14a063868d37c5601b7b...
```

#### Retrieve block bytes

* `GET /blocks/:slot/:hash`: Retrieves raw hex-encoded CBOR bytes of a block at given `:slot` and with given `:hash` (aka. _point_)
  * `200` : Returns hex-encoded bytes for the block serialised as CBOR
  * `400` : Wrongly formatted `:slot` or `:hash` paths
  * `404` : No block exists at given point

**Example** (result is truncated for legibility, and the block is empty):

```
% curl -v http://localhost:9003/blocks/69206375/6f99b5f3deaeae8dc43fce3db2f3cd36ad8ed174ca3400b5b1bed76fdf248912
820685828a0c19012...8080a080
```

#### List snapshots

* `GET /snapshots`: List all the points for which this db-server has a ledger state snapshot
  * `200` : Returns JSON-encoded list of points

**Example** (result is truncated for legibility, and the block is empty):

```
% curl -v http://localhost:9003/snapshots | jq .
...
  {
    "hash": "82499f71f6ab9dda3f84023897ca1e3cb3fe5c19b7018be2e55df57b191713a0",
    "slot": 59735
  },
  {
    "hash": "574a6ed18ccca232028a4b2632abdb7b99ef28adf25e58ee7392574fe69a4c74",
    "slot": 59737
  },
  {
    "hash": "578cbb520f441b7d1530dac16844302c3e5e9ba4c434b439359c89c62a08cd69",
    "slot": 59747
  }
]
```

#### Retrieve snasphot

* `GET /snapshots/:slot`: Retrieve raw hex-encoded CBOR bytes of ledger state at given `:slot`, if it exists
  * `200` : Returns hex-encoded bytes for the snapshot serialised as CBOR
  * `400` : Wrongly formatted `:slot`
  * `404` : No snapshot exists at given slot


**Example** (result is truncated for legibility, and the block is empty):

```
% curl -v http://localhost:9003/snapshots/59737
...
76ba082c604e6af7c482f716934ed08d19c227331b00071a1474e7f000f6
```
