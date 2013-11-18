{ stdenv }:

stdenv.mkDerivation {
  name = "homepage";

  src = ./pkg/homepage.tar.bz2;

  phases = [ "unpackPhase" "installPhase" ];
  
  installPhase =
    ''
      mkdir -p $out
      cp -prvd * $out/
    '';
}