{ mkDerivation, base, binary, bytestring, containers, hpack, HsYAML
, lib, mtl, string-interpolate, tasty, tasty-hunit
, tasty-quickcheck, text
}:
mkDerivation {
  pname = "hyperpipe";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base binary bytestring containers HsYAML mtl text
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base binary bytestring containers HsYAML mtl text
  ];
  testHaskellDepends = [
    base binary bytestring containers HsYAML mtl string-interpolate
    tasty tasty-hunit tasty-quickcheck text
  ];
  prePatch = "hpack";
  license = lib.licenses.bsd3;
}
