with (import (builtins.fetchTarball { 
  url = "https://github.com/dmjio/miso/archive/1.5.2.tar.gz";
  sha256 = "11rckm7hm2xmxn3w90fmyd7s4q64fvapvq53z5pyc13x36z4dyb3";
}) {});
let ghc = pkgs.haskell.packages.ghcjs86.ghcWithPackages(pkgs: with pkgs; [ghcjs-base]);
    inherit (pkgs) closurecompiler;
in 
pkgs.mkShell {
  buildInputs = [ghc closurecompiler];
}
