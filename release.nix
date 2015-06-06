{ backhandSrc ? { outPath = ./.; revCount = 0; shortRev = "abcdef"; rev = "HEAD"; }
, officialRelease ? false
}:

let
  backhand = h: with import <nixpkgs> {}; with h; mkDerivation {
    pname = "backhand";
    version = "0.0.0.0";
    src = backhandSrc;
    isLibrary = false;
    isExecutable = true;
    buildDepends = [
      base cabal-install
      async stm stm-chans mtl transformers lifted-base monad-control resourcet
      containers unordered-containers ixset time text bytestring aeson
      websockets netwire
    ];
    description = "Back-end connection management system for Internet card games";
    license = stdenv.lib.licenses.mit;
  };
in rec {
  build = {
    ghc7-10-1 = with import <nixpkgs> {}; backhand haskell.packages.ghc7101;
    ghc7-8-4 = with import <nixpkgs> {}; backhand haskell.packages.ghc784;
  };
}
