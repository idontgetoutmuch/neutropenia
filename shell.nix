{ pkgs ? import <nixpkgs> {}, compiler ? "ghc822", doBenchmark ? false }:

let

f = { mkDerivation, haskell, base, foldl, Frames, fuzzyset
    , inline-r, integration, lens, libintlOrEmpty
    , pandoc-types, plots
    , diagrams-rasterific
    , diagrams
    , diagrams-svg
    , diagrams-contrib
    , dimensional
    , hmatrix
    , hmatrix-sundials
    , monad-loops
    , R, random, sundials, stdenv
    , template-haskell, temporary }:
mkDerivation {
  pname = "mrp";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base
    diagrams
    diagrams-rasterific
    diagrams-svg
    diagrams-contrib
    dimensional
    (haskell.lib.dontCheck inline-r)
    foldl
    Frames
    fuzzyset
    hmatrix
    # hmatrix-sundials
    integration
    lens
    monad-loops
    pandoc-types
    plots
    random
    template-haskell
    temporary
  ];
  buildDepends = [ libintlOrEmpty ];
  executableSystemDepends = [
    R
    pkgs.rPackages.dplyr
    pkgs.rPackages.ggmap
    pkgs.rPackages.ggplot2
    pkgs.rPackages.knitr
    pkgs.rPackages.maptools
    pkgs.rPackages.reshape2
    pkgs.rPackages.rgeos
    pkgs.rPackages.rgdal
    pkgs.rPackages.rstan
    pkgs.rPackages.zoo
    sundials];
  license = stdenv.lib.licenses.bsd3;
};

haskellPackages = if compiler == "default"
  then pkgs.haskellPackages
  else pkgs.haskell.packages.${compiler};

# Fixup a few things
myHaskellPackages = haskellPackages.override {
  overrides = self: super: with pkgs.haskell.lib; {
    diagrams-rasterific = doJailbreak super.diagrams-rasterific;
    diagrams-svg = doJailbreak super.diagrams-svg;
    diagrams-contrib = doJailbreak super.diagrams-contrib;
    diagrams = doJailbreak super.diagrams;
    inline-r = dontCheck super.inline-r;
    pipes-group = doJailbreak super.pipes-group;
    hmatrix = patched-hmatrix super;
    hmatrix-sundials = patched-hmatrix-sundials super;
  };
};

patched-hmatrix = ps: ps.hmatrix.overrideAttrs (oldAttrs: rec {
  src = pkgs.fetchgit {
    url = git://github.com/haskell-numerics/hmatrix;
    rev = "94c7f89929e09ca6e98976cb59dec574ee164e20";
    sha256 = "0mm7r81ajb38s0aqa2ywbc1c3zl4j4cffiy77iwmp5qaj53jz0d8";
  };
  postUnpack = ''
    sourceRoot=''${sourceRoot}/packages/base
    echo Source root reset to ''${sourceRoot}
  '';
});

patched-hmatrix-sundials = ps: pkgs.haskell.lib.addPkgconfigDepend (ps.hmatrix-sundials.overrideAttrs (oldAttrs: rec {
  src = pkgs.fetchgit {
    url = git://github.com/haskell-numerics/hmatrix;
    rev = "94c7f89929e09ca6e98976cb59dec574ee164e20";
    sha256 = "0mm7r81ajb38s0aqa2ywbc1c3zl4j4cffiy77iwmp5qaj53jz0d8";
  };
  postUnpack = ''
    sourceRoot=''${sourceRoot}/packages/sundials
    echo Source root reset to ''${sourceRoot}
  '';
})) pkgs.sundials;

variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

drv = variant (myHaskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
