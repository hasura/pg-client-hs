{ gitignore, pkgs, ... }:

let
  inherit (gitignore) gitignoreSource;
  inherit (pkgs.haskell-nix) cabalProject;
  inherit (pkgs.haskell-nix.haskellLib) cleanGit;
in
cabalProject {
  src = gitignoreSource ../.;
  compiler-nix-name = "ghc8105";
}
