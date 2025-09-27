class DbServer < Formula
  desc "ReST-ish server and CLI tool to expose Cardano-node's DB"
  homepage "https://github.com/pragma-org/db-server"
  url "https://github.com/pragma-org/db-server/archive/refs/tags/v0.1.0.tar.gz"
  sha256 "PLACEHOLDER_SHA256"
  license "Apache-2.0"

  depends_on "cabal-install" => :build
  depends_on "ghc" => :build
  depends_on "pkg-config" => :build
  depends_on "libsodium"
  depends_on "secp256k1"

  def install
    # Run the custom build script which handles native dependencies
    system "./build.hs"
    
    # Install the binary
    bin.install ".cabal-sandbox/bin/db-server" => "db-server"
  end

  test do
    # Test that the binary exists and shows help
    assert_match "db-server", shell_output("#{bin}/db-server --help")
  end
end
