{ mkDerivation, base, basic-prelude, containers, libsystemd-journal
, pipes, pipes-safe, reactive-banana, stdenv, time
, unordered-containers
}:
mkDerivation {
  pname = "frp0";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    base basic-prelude containers libsystemd-journal pipes pipes-safe
    reactive-banana time unordered-containers
  ];
  license = stdenv.lib.licenses.unfree;
}
