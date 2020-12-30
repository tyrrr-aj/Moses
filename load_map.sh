#!/bin/bash

. moses.config

docker stop mosesdb
docker rm mosesdb

docker run -p 5432:5432 -e 'POSTGRES_PASSWORD='$postgres_password -d --name mosesdb mosesdb_server:latest
docker run -v $map_dir:/data --network="host" --name mosesdb_loader mosesdb_loader:latest