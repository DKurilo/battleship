#!/bin/sh
docker-compose up
ssh dima@battleship-fp.com 'rm -rf /var/battleship/site'
scp -r ./out dima@battleship-fp.com:/var/battleship/site
ssh -t dima@battleship-fp.com 'cp -r /var/battleship/local/* /var/battleship/site/public/; chown -R dima:battleship /var/battleship; chmod -R 750 /var/battleship; chmod -R 770 /var/battleship/log; cd /var/battleship; sudo setcap CAP_NET_BIND_SERVICE=+eip /var/battleship/site/battleship; sudo service supervisor restart'
