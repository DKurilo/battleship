# Battleship

[![Build Status](https://travis-ci.org/DKurilo/battleship.svg?branch=master)](https://travis-ci.org/DKurilo/battleship)

Pure.. Functional..  
Haskell + Snap + MongoDB + TypeScript + React  
https://battleship-fp.com/  
  
Actualy, client has some impurity. It uses not immutable state. I don't know how to avoid it. Maybe you can help me.  
  
To build client:  
```
yarn install
yarn build
```
  
To build server:  
Copy `devel.cfg.template` to `devel.cfg`. Fix parameters.  
```
stack build
server/run_ssl.sh
stack run battleship-bot
```
  
## SSL

In case you want to use HTTPS:  
Run `./prepare_with_ssl.sh`. it will create ssl keys.
To run server `./run_ssl.sh` and then in browser: `https://localhost:9443/`  

## Build for Linux

To build server for linux you can use special docker image.  
Just run `docker-composer up` from deploy folder. Check `./deploy/build.sh` for more information.  

## MongoDB

With admin access:  

```
use battleship;
db.createUser({user:"battleshipuser", pwd: "your_password",roles:[{role:"readWrite",db:"battleship"}]});
db.createCollection("games");
db.createCollection("chats");
use battleshipbot;
db.createUser({user:"battleshipbotuser", pwd: "your_password",roles:[{role:"readWrite",db:"battleshipbot"}]});
db.createCollection("games");
```

## ToDo

Tests. For everything!  
