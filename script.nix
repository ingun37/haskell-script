{ mkDerivation, aeson, base, base16-bytestring, bytestring, cond
, containers, cryptohash-sha1, directory, directory-tree, filepath
, hspec, QuickCheck, regexpr, split, stdenv, utf8-string
}:
mkDerivation {
  pname = "script";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base base16-bytestring bytestring cond containers
    cryptohash-sha1 directory directory-tree filepath regexpr split
    utf8-string
  ];
  executableHaskellDepends = [
    aeson base base16-bytestring bytestring cond containers
    cryptohash-sha1 directory directory-tree filepath regexpr split
    utf8-string
  ];
  testHaskellDepends = [
    aeson base base16-bytestring bytestring cond containers
    cryptohash-sha1 directory directory-tree filepath hspec QuickCheck
    regexpr split utf8-string
  ];
  doHaddock = false;
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
