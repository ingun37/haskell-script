#!/bin/zsh
cabal2nix . > script.nix
nix-build