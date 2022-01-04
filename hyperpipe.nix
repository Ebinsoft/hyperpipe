{ mkDerivation, base, bytestring, containers, hpack, HsYAML, lib
, mtl, optparse-applicative, pcap, persist, string-interpolate
, tasty, tasty-hunit, tasty-quickcheck, text, time, unagi-chan
}:
mkDerivation {
  pname = "hyperpipe";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring containers HsYAML mtl pcap persist text time
    unagi-chan
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base bytestring containers HsYAML mtl optparse-applicative pcap
    persist text time unagi-chan
  ];
  testHaskellDepends = [
    base bytestring containers HsYAML mtl pcap persist
    string-interpolate tasty tasty-hunit tasty-quickcheck text time
    unagi-chan
  ];
  prePatch = "hpack";
  license = lib.licenses.bsd3;
}
