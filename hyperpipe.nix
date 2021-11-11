{ mkDerivation, base, binary, bytestring, containers, hpack, HsYAML
, lib, string-interpolate, tasty, tasty-hunit, tasty-quickcheck
, text
}:
mkDerivation {
  pname = "hyperpipe";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base binary bytestring containers HsYAML text
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base binary bytestring containers HsYAML text
  ];
  testHaskellDepends = [
    base binary bytestring containers HsYAML string-interpolate tasty
    tasty-hunit tasty-quickcheck text
  ];
  prePatch = "hpack";
  license = lib.licenses.bsd3;
}
