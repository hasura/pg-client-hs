{
  description = "A low-level PostgreSQL client library for Haskell";

  inputs = {
    ####################
    # Misc. utilities. #
    ####################
    # Compatibility shim to build this system on systems without Flakes support.
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    # Unofficial library of utilities for managing with Nix Flakes.
    flake-utils.url = "github:numtide/flake-utils";
    # Utility to clean source paths according to gitignore files.
    gitignoreSrc = {
      url = "github:hercules-ci/gitignore.nix";
      flake = false;
    };
    ##########################################
    # 'haskell.nix' Framework & Dependencies #
    ##########################################
    # Regularly updated 'Nix-ified' snapshots of the Hackage package index.
    hackage = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };
    # Regularly updated 'Nix-ified' Stackage snapshots.
    stackage = {
      url = "github:input-output-hk/stackage.nix";
      flake = false;
    };
    # IOHK's 'haskell.nix' framework.
    #
    # NOTE: Passing in our own Hackage & Stackage snapshots as arguments allows
    # us to manage package versioning separately from the upstream IOHK repo.
    haskell-nix = {
      inputs.hackage.follows = "hackage";
      inputs.stackage.follows = "stackage";
      url = "github:input-output-hk/haskell.nix";
    };

    ####################
    # Nix Package Sets #
    ####################
    # NOTE: We have to be careful to source our package sets from the same
    # place that 'haskell.nix' does.
    #
    # Without this, we won't be able to pull from their binary cache, and will
    # very likely end up having to build multiple copies of GHC to bring up the
    # development environment.
    nixpkgs.follows = "haskell-nix/nixpkgs-2105";
    unstable.follows = "haskell-nix/nixpkgs-unstable";
  };

  outputs = inputs@{ self, flake-utils, haskell-nix, nixpkgs, ... }:
    let
      inherit (haskell-nix.internal) config;
      inherit (flake-utils.lib) eachSystem;
      # All of the systems we want to build development environments for.
      systems = [
        "x86_64-linux"
        "x86_64-darwin"
      ];
      # All of the overlays we want to apply to the base Nix package set.
      overlays = [
        haskell-nix.overlay
        (import ./nix/hls-overlay.nix)
        (import ./nix/pg-client-overlay.nix { inherit (inputs) gitignoreSrc; })
      ];
    in
    flake-utils.lib.eachSystem systems (system:
      let
        pkgs = import nixpkgs { inherit config overlays system; };
        flake = pkgs.pg-client.flake { };
      in
      {
        # NOTE: We can't pass the entirety of 'flake' through here due to
        # a bug somewhere between haskell.nix and the Nix CLI itself.
        #
        # cf. https://github.com/input-output-hk/haskell.nix/issues/1097
        inherit (flake) packages;
        defaultPackage = flake.packages."pg-client:lib:pg-client";
        devShell = pkgs.pg-client.shellFor {
          # NOTE: Workaround for https://github.com/input-output-hk/haskell.nix/issues/590#issuecomment-712702992
          additional = hpkgs: [ hpkgs.pg-client ];
          withHoogle = true;
          tools = {
            cabal = "3.4.0.0";
            haskell-language-server = "1.3.0.0";
          };
        };
      }
    );
}
