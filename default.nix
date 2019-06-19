{ mkDerivation, array, base, bytestring, containers, stdenv, text
, unordered-containers, vector
}:
mkDerivation {
  pname = "haskexp";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    array base bytestring containers text unordered-containers vector
  ];
  license = stdenv.lib.licenses.asl20;
}
