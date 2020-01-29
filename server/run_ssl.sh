#!/bin/sh
stack run battleship -- --ssl-port=9443 --ssl-cert=./localhost_certificate.pem --ssl-key=./localhost_key.pem \
                        --ssl-address=0.0.0.0 --no-ssl-chain-cert --port=9000 --address=127.0.0.1
