{ mkDerivation, aeson, array, base, bytestring, containers
, scientific, stdenv, text, unordered-containers, vector
}:
mkDerivation {
  pname = "haskexp";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson array base bytestring containers scientific text
    unordered-containers vector
  ];
  executableHaskellDepends = [
    aeson array base bytestring containers scientific text
    unordered-containers vector
  ];
  license = stdenv.lib.licenses.asl20;
}
