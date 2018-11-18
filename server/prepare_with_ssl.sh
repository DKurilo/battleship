#!/bin/sh
openssl req -newkey rsa:4096 -nodes -keyout localhost_key.pem -x509 -days 365 -out localhost_certificate.pem \
            -subj "/C=US/ST=Denial/L=Springfield/O=Dis/CN=localhost"
cabal install snap snap-server -fopenssl --reinstall