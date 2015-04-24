{ stdenv, haskellngPackages }:

let
  env = haskellngPackages.ghcWithPackages (p: with p; [
    aeson
    esqueleto
    bytestring
    cabal-install
    classy-prelude
    classy-prelude-conduit
    classy-prelude-yesod
    conduit
    containers
    data-default
    directory
    fast-logger
    file-embed
    hjsmin
    http-conduit
    monad-control
    monad-logger
    persistent
    persistent-postgresql
    persistent-template
    regex-compat
    safe
    shakespeare
    template-haskell
    text
    time
    unordered-containers
    vector
    wai-extra
    wai-logger
    warp
    yaml
    yesod
    yesod-auth
    yesod-bin
    yesod-core
    yesod-form
    yesod-static
    yesod-markdown
    yesod-sitemap
  ]);
in
  stdenv.mkDerivation {
    name        = "project-name";
    buildInputs = [env];
    shellHook   = ''
      export NIX_GHC="${env}/bin/ghc"
      export NIX_GHCPKG="${env}/bin/ghc-pkg"
      export NIX_GHC_DOCDIR="${env}/share/doc/ghc/html"
      export NIX_GHC_LIBDIR=$( $NIX_GHC --print-libdir )
    '';
  }
