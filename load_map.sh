#!/bin/bash

. moses.config

if docker ps -a | grep -wq mosesdb;
then
	docker stop mosesdb
	docker rm mosesdb
fi

docker run -p 5432:5432 -e 'POSTGRES_PASSWORD='$postgres_password -d --name mosesdb mosesdb_server:latest

sleep 5s

docker run -v $map_dir:/data --rm --network="host" --name mosesdb_loader mosesdb_loader:latest
