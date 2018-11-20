# Battleship
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
cabal install
cabal run --p 9000
```
  
In browser: `http://localhost:9000/`

## SSL

In case you want to use HTTPS:  
After `cabal install` run: `ghc-pkg unregister -f snap-server` and then `./prepare_with_ssl.sh`. it will create ssl keys and build snap with `-fopenssl` option.
To run server `./run_ssl.sh` and then in browser: `https://localhost:9443/`  

## Build for Linux

To build server for linux you can use special docker image.  
Just run `docker-composer up` from deploy folder.  
