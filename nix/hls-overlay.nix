final: prev:

# NOTE: Unfortunately, haskell-language-server-1.3.0.0 cannot be installed from
# Hackage due to a source dependency.
#
# This overlay acts as a workaround, pulling the git rev associated with the
# 1.3.0.0 tagged release and mapping its source dependencies with a SHA that
# allows Nix to build them.
#
# It's a little convoluted, but the net benefit is that all users should get a
# cached copy of haskell-language-server when they load up this project shell.

let
  inherit (prev.haskell-nix) cabalProject;
in
{
  haskell-nix = prev.haskell-nix // {
    custom-tools = prev.haskell-nix.custom-tools // {
      haskell-language-server."1.3.0.0" = oldArgs:
        let
          args = removeAttrs oldArgs [ "version" ];
          drv = cabalProject (args // {
            name = "haskell-language-server";
            src = prev.fetchFromGitHub {
              owner = "haskell";
              repo = "haskell-language-server";
              rev = "d7a745e9b5ae76a4bf4ee79a9fdf41cf6f1662bf";
              sha256 = "0rxnkijdvglhamqfn8krsnnpj3s7kz2v5n5ndy37a41l161jqczx";
            };
            sha256map = {
              # Map the git rev for this source dependency to its sha256 hash.
              "https://github.com/hsyl20/ghc-api-compat" = {
                "8fee87eac97a538dbe81ff1ab18cff10f2f9fa15" =
                  "16bibb7f3s2sxdvdy2mq6w1nj1lc8zhms54lwmj17ijhvjys29vg";
              };
            };
          });

        in
        drv.haskell-language-server.components.exes.haskell-language-server;
    };
  };
}
