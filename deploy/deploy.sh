#!/bin/sh
docker-compose up
ssh dima@battleship-fp.com 'rm -rf /var/battleship/site'
find ./out -type f -name ".DS_Store" -exec rm {} \;
scp -r ./out dima@battleship-fp.com:/var/battleship/site
ssh -t dima@battleship-fp.com 'cp -r /var/battleship/local/* /var/battleship/site/public/; chown -R dima:battleship /var/battleship/site; chmod -R 750 /var/battleship/site; cd /var/battleship; sudo setcap CAP_NET_BIND_SERVICE=+eip /var/battleship/site/battleship; sudo service supervisor restart'
