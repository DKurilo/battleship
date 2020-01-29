#!/bin/sh
cd /var/build/
rm -rf ./deploy/out
mkdir -p ./deploy/out

cd /var/build/client
yarn install --force
yarn build:prod
cp  -r ./build ../deploy/out/public

cd ..

stack install --local-bin-path ../deploy/out/

cp ./rules.json ../deploy/out/
cp ./rules.json ../deploy/out/rules.bot.json
