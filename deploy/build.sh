#!/bin/sh
cd /var/build/
rm -rf ./deploy/out
mkdir -p ./deploy/out

export CI=true
export NODE_ENV=production

cd /var/build/client
yarn install --force
yarn build --frozen-lockfile
cp  -r ./build ../deploy/out/public

cd ..

stack install --local-bin-path ./deploy/out/

cp ./server/rules.json ./deploy/out/
cp ./bot/rules.json ./deploy/out/rules.bot.json

cd ./deploy
