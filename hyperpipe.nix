{ mkDerivation, base, binary, bytestring, containers, hpack, HsYAML
, lib, mtl, optparse-applicative, pcap, string-interpolate, tasty
, tasty-hunit, tasty-quickcheck, text, unagi-chan
}:
mkDerivation {
  pname = "hyperpipe";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base binary bytestring containers HsYAML mtl pcap text unagi-chan
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base binary bytestring containers HsYAML mtl optparse-applicative
    pcap text unagi-chan
  ];
  testHaskellDepends = [
    base binary bytestring containers HsYAML mtl pcap
    string-interpolate tasty tasty-hunit tasty-quickcheck text
    unagi-chan
  ];
  prePatch = "hpack";
  license = lib.licenses.bsd3;
}
