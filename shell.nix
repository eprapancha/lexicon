{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    # Java 21 (required for shadow-cljs)
    jdk21

    # Clojure tooling
    clojure

    # Node.js and npm
    nodejs_20

    # Rust toolchain for WASM
    rustc
    cargo
    wasm-pack

    # Build tools
    pkg-config
  ];

  shellHook = ''
    echo "🚀 Lexicon development environment loaded!"
    echo ""
    echo "Java version: $(java -version 2>&1 | head -n 1)"
    echo "Node version: $(node --version)"
    echo "Rust version: $(rustc --version)"
    echo ""
    echo "Quick start:"
    echo "  1. npm install"
    echo "  2. ./scripts/build-wasm.sh"
    echo "  3. cd packages/lexicon-bridge && node index.js"
    echo "  4. (new terminal) cd packages/editor-cljs && npx shadow-cljs watch app"
    echo ""
  '';
}
