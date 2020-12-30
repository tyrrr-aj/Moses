#!/bin/bash

docker build ./Map/server -t mosesdb_server:latest
docker build ./Map/loader -t mosesdb_loader:latest

chmod +x Server/moses/_build/default/rel/moses_server/bin/moses_server