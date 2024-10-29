_default:
  @just --list

build:
  cabal build all

test:
  cabal test all --test-show-details=direct

format:
    fourmolu -i src

gen-haddock:
    cabal haddock bluefin-log-base --enable-documentation

gen-haddock-hackage:
    cabal haddock bluefin-log-base --enable-documentation --haddock-for-hackage
