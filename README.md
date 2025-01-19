# db-server

A ReST-ish server to expose Cardano-node's DB.

## Build

### Prerequisites

* Install Haskell toolchain, for example using [GHCUp](https://www.haskell.org/ghcup/) or Nix
* Install Cardano node's system dependencies (see [cardano-node wiki](https://github.com/input-output-hk/cardano-node-wiki/wiki/install)

### Building

```
cabal update
cabal build all
```

### Testing

```
cabal test
```

## Usage

### Starting the server

To run `db-server` over a `preprod` network database:

* Follow instructions from [Mithril website](https://mithril.network/doc/manual/getting-started/bootstrap-cardano-node) to retrieve a preprod database using [_Preprod_ configuration](https://mithril.network/doc/manual/getting-started/network-configurations). If all goes well, you should have a directory containing a primed database for example in `./db`
* Retrieve all configuration files for `Preprod` network from the [environment](https://book.world.dev.cardano.org/env-preprod.html) page into a directory, say `./config`

Then from the source directory of `db-server`, starts the server with the following command:

```
cabal run db-server -- --db db --config config/config.json
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

* `GET /:slot/:hash/header`: Retrieves raw hex-encoded bytes of a block header at given `:slot` and with given `:hash` (aka. _point_)
  * `200` : Returns the raw hex-encoded bytes for header
  * `400` : Wrongly formatted `:slot` or `:hash` paths
  * `404` : No block exists at given point

**Example** (result truncated for legibility):

```
% curl -v http://localhost:9003/69206375/6f99b5f3deaeae8dc43fce3db2f3cd36ad8ed174ca3400b5b1bed76fdf248912/header
828a1a0028375b1a0420016758209694aa14a063868d37c5601b7b...
```

#### Retrieve block bytes

* `GET /:slot/:hash`: Retrieves raw hex-encoded CBOR bytes of a block at given `:slot` and with given `:hash` (aka. _point_)
  * `200` : Returns hex-encoded bytes for the block serialised as CBOR
  * `400` : Wrongly formatted `:slot` or `:hash` paths
  * `404` : No block exists at given point

**Example** (result is truncated for legibility, and the block is empty):

```
% curl -v http://localhost:9003/69206375/6f99b5f3deaeae8dc43fce3db2f3cd36ad8ed174ca3400b5b1bed76fdf248912
820685828a0c19012...8080a080
```
