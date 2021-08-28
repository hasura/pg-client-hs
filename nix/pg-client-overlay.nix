{ gitignoreSrc }:

final: prev:

let
  inherit (prev.haskell-nix) cabalProject;
  inherit (prev.callPackage gitignoreSrc { }) gitignoreSource;
in
{
  pg-client = cabalProject {
    src = gitignoreSource ../.;
    compiler-nix-name = "ghc8105";
  };
}
