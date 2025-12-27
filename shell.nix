{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    # Java 21 (required for shadow-cljs)
    jdk21

    # test gh action locally
    act

    # Clojure tooling
    clojure
    babashka

    # Node.js and npm
    nodejs_20
    tree-sitter  # Tree-sitter CLI for grammar builds

    # Rust toolchain for WASM
    rustc
    cargo
    wasm-pack
    lld  # LLVM linker (required for wasm32 target)

    # Build tools
    pkg-config
  ];

  shellHook = ''
    echo "🚀 Lexicon development environment loaded!"
    echo ""
    echo "Java version: $(java -version 2>&1 | head -n 1)"
    echo "Node version: $(node --version)"
    echo "Rust version: $(rustc --version)"
    echo "Babashka version: $(bb --version 2>&1 || echo 'not installed')"
    echo ""
    echo "Quick start:"
    echo "  bb dev           # Start development environment"
    echo "  bb build         # Full production build"
    echo "  bb ci-test       # Test CI workflow locally"
    echo ""
    echo "See 'bb tasks' for all available tasks"
    echo ""
  '';
}
