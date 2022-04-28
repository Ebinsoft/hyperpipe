{ mkDerivation, base, brick, bytestring, cereal, containers
, grpc-haskell, hpack, HsYAML, lib, mtl, optparse-applicative, pcap
, string-interpolate, tasty, tasty-hunit, tasty-quickcheck, text
, time, unagi-chan
}:
mkDerivation {
  pname = "hyperpipe";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring cereal containers HsYAML mtl pcap text time
    unagi-chan
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base brick bytestring cereal containers grpc-haskell HsYAML mtl
    optparse-applicative pcap text time unagi-chan
  ];
  testHaskellDepends = [
    base bytestring cereal containers HsYAML mtl pcap
    string-interpolate tasty tasty-hunit tasty-quickcheck text time
    unagi-chan
  ];
  prePatch = "hpack";
  license = lib.licenses.bsd3;
}
