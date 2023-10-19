{ nixpkgs ? import <nixpkgs> {} }:
let
  nixpkgs_source = fetchTarball https://github.com/NixOS/nixpkgs/archive/nixos-23.05.tar.gz;
   overlays = [];
   config = {
     allowUnfree = true;
   };
   myNix = import nixpkgs_source {inherit overlays; inherit config;};
in
with myNix.pkgs; 
let hp94 = haskell.packages.ghc943;
    hp92 = haskell.packages.ghc925;
    hp = hp94;
    ghc = hp.ghcWithPackages (ps: with ps; ([
      cabal-install
      Agda
      filemanip
      pretty
      containers
      aeson
      aeson-pretty
      file-embed
      bytestring
      mtl
      directory
      filepath
      deepseq
      async
    ]));
    myAgda = agda.withPackages (p: [ p.standard-library ]);

in pkgs.stdenv.mkDerivation {
  name = "my-env-0";
  buildInputs = [ glibcLocales zip ghc
                  # myAgda
                ]; # git does not work easily here.
  shellHook = ''
    export LANG=en_US.UTF-8
    eval $(egrep ^export ${ghc}/bin/ghc)
  '';
}


