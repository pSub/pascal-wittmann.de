{ mkDerivation, aeson, base, blaze-html, blaze-markup, bytestring
, classy-prelude, classy-prelude-conduit, classy-prelude-yesod
, clientsession, conduit, containers, data-default, directory
, esqueleto, fast-logger, file-embed, filepath, hjsmin, hspec
, http-conduit, monad-control, monad-logger, old-locale, persistent
, persistent-postgresql, persistent-template, regex-compat
, resourcet, safe, shakespeare, lib, template-haskell, text
, time, transformers, unordered-containers, vector, wai-extra
, wai-logger, warp, yaml, yesod, yesod-auth, yesod-core, yesod-form
, yesod-markdown, yesod-newsfeed, yesod-persistent, yesod-sitemap
, yesod-static, yesod-test, cabal-install, yesod-bin, load-env
, yesod-auth-oauth2, yesod-csp, ghcid, foreign-store
}:
mkDerivation {
  pname = "homepage";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  doHaddock = false; # https://github.com/haskell/haddock/issues/775
  buildTools = [ cabal-install yesod-bin ghcid ];
  libraryHaskellDepends = [
    aeson base blaze-html blaze-markup bytestring classy-prelude
    classy-prelude-conduit classy-prelude-yesod clientsession conduit
    containers data-default directory esqueleto fast-logger file-embed
    filepath hjsmin http-conduit monad-control monad-logger old-locale
    persistent persistent-postgresql persistent-template regex-compat
    safe shakespeare template-haskell text time unordered-containers
    vector wai-extra wai-logger warp yaml yesod yesod-auth yesod-core
    yesod-form yesod-markdown yesod-newsfeed yesod-persistent
    yesod-sitemap yesod-static load-env yesod-auth-oauth2 yesod-csp
    foreign-store
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    aeson base classy-prelude classy-prelude-yesod hspec monad-logger
    persistent persistent-postgresql resourcet shakespeare transformers
    yesod yesod-core yesod-test
  ];
  homepage = "https://www.pascal-wittmann.de";
  description = "Personal website";
  license = lib.licenses.bsd3;
}
