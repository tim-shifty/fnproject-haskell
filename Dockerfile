FROM debian:bullseye

ENV LANG C.UTF-8

WORKDIR /function

COPY ./haskell-hello.cabal /function/haskell-hello.cabal

# Workaround cabal package DB issue #5516 (should be fixed in cabal 2.4.1)
RUN mkdir -p $HOME/.cabal/store/ghc-8.6.5/package.db

RUN apt-get update && apt-get install -y --no-install-recommends haskell-platform pkg-config
RUN cabal new-update && cabal new-install cabal-install
RUN cabal new-install --only-dependencies -j4

COPY app/ /function/app/
COPY LICENSE /function/LICENSE
RUN cabal new-configure
RUN cabal new-build
RUN cabal new-install

COPY func.yaml /function/func.yaml

CMD ["/function/dist-newstyle/build/x86_64-linux/ghc-8.6.5/haskell-hello-0.1.0.0/x/haskell-hello/build/haskell-hello/haskell-hello"]
