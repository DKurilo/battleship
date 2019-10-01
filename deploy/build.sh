#!/bin/sh
cd /var/build/
rm -rf ./deploy/out
mkdir -p ./deploy/out

cd /var/build/client
yarn install --force
yarn build:prod
cp  -r ./build ../deploy/out/public

cabal update
ghc-pkg unregister --force snap-server
rm -rf /var/snap-server
mkdir -p /var/snap-server && cd /var/snap-server
git clone https://github.com/DKurilo/snap-server.git .
cabal install -fopenssl --builddir=dist.prod --force-reinstalls
cd /var/build/server
cabal install --builddir=dist.prod
cabal install snap --builddir=dist.prod --force-reinstalls
cabal build --builddir=dist.prod

cp ./dist.prod/build/battleship/battleship ../deploy/out/

cp ./rules.json ../deploy/out/

cd ../bot
cabal update
cabal install --builddir=dist.prod
cabal build --builddir=dist.prod

cp ./dist.prod/build/battleship-bot/battleship-bot ../deploy/out/
cp ./rules.json ../deploy/out/rules.bot.json

cd ..
