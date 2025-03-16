{ pkgs, ... }:

let ghcVersion = "ghc96"; # Change if you need a different GHC version
in {
  languages.haskell.enable = true;
  languages.haskell.package = pkgs.haskell.packages.${ghcVersion}.ghc;

  packages = with pkgs.haskell.packages.${ghcVersion};
    [ fourmolu ] ++ [
      pkgs.sqlite # SQLite CLI for debugging
    ];

  # Format-on-save integration
  env.FOURMOLU_OPTIONS = "--stdin-input-file";

  # Run SQLite with `sqlite3 mydb.sqlite` for easy debugging
  # shell.hooks.postShellHook = ''
  #   echo "Haskell dev environment ready!"
  #   echo "- SQLite available via 'sqlite3'"
  #   echo "- Format Haskell code with 'fourmolu --mode inplace <file>'"
  # '';
}
