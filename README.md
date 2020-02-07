# haskell-hello

This is the first attempt at running Haskell under the Fn server.

Yet to do:

* Running a Haskell executable using the Docker runtime
* Separating the FDK part from the function itself
* Making the FDK part compliant with the Fn specification
* Publishing the FDK as a Haskell package
* Making the `fn` command line tool accept haskell as a runtime on branch of fnproject

I am using Stack to build for local development, but Cabal within the Dockerfile.
