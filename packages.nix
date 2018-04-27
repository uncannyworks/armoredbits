{ mkDerivation, apecs, base, bytestring, cborg, clock, containers
, microlens-platform, network, network-simple, serialise, stdenv
, stm, tagged, text
}:
mkDerivation {
  pname = "armoredbits";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    apecs base bytestring cborg clock containers microlens-platform
    network network-simple serialise stm tagged text
  ];
  executableHaskellDepends = [ base ];
  homepage = "http://armoredbits.com";
  description = "Realtime Multiplayer AI Mech Battles";
  license = stdenv.lib.licenses.asl20;
}
