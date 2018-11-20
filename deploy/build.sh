#!/bin/sh
cd /var/build/
rm -rf ./deploy/out
mkdir -p ./deploy/out

cd /var/build/client
yarn install
yarn build:prod
cp  -r ./build ../deploy/out/public

cd ../server
cabal update
cabal install
ghc-pkg unregister --force snap-server
cabal install snap-server -fopenssl

cp ./dist/build/battleship/battleship ../deploy/out/

cp ./rules.json ../deploy/out/

cd ..
