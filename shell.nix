{ compiler ? "ghc822" }:

(import ./. { inherit compiler; }).env
