{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }: let
    system = "x86_64-linux";
    pkgs = import nixpkgs {inherit system;};
  in  {
    devShells.${system}.default = pkgs.mkShellNoCC {
      packages = with pkgs; [
        (ghc.withPackages (hs: with hs; [
          cabal-install
          haskell-language-server
        ]))
      ];
    };
  };
}
