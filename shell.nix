
with import <nixpkgs> {};
let
    deps = hs: with hs; [ megaparsec ghcide ];

in mkShell {
    buildInputs = [ (ghc.withPackages deps) ];
}
