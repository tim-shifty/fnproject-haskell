# haskell-hello

This is an attempt at running Haskell under the Fn server.

* Uses the Docker runtime

Yet to do:

* Separating the FDK part from the function itself
* Making the FDK part compliant with the Fn specification
* Publishing the FDK as a Haskell package
* Making the `fn` command line tool accept haskell as a runtime on branch of fnproject

I am using Stack to build for local development, but Cabal within the Dockerfile.
