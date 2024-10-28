_default:
  @just --list

# Build package.
build:
  cabal build all

# Test package.
test:
  cabal test all --test-show-details=direct

format:
    fourmolu -i src
