FROM haskell:8.0

RUN cabal update && cabal install HUnit
RUN mkdir /src
WORKDIR /src
CMD ["ghci", "intpart"]
