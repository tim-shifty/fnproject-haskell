FROM debian:bullseye

ENV LANG C.UTF-8

WORKDIR /function

COPY ./haskell-hello.cabal /function/haskell-hello.cabal

# Workaround cabal package DB issue #5516 (should be fixed in cabal 2.4.1)
RUN mkdir -p $HOME/.cabal/store/ghc-8.6.5/package.db

RUN apt-get update && apt-get install -y --no-install-recommends haskell-platform && cabal new-update && cabal new-install --only-dependencies -j4

COPY app/ /function/app/
RUN cabal new-install --bindir=/function/bin --datadir=/function/data

COPY func.yaml /function/func.yaml
CMD ["/function/bin/haskell-hello"]
