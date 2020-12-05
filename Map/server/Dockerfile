FROM pgrouting/pgrouting

RUN su postgres -c "psql -c 'CREATE DATABASE osm'" \
 && su postgres -c "psql --dbname osm -c 'CREATE EXTENSION postgis'" \
 && su postgres -c "psql --dbname osm -c 'CREATE EXTENSION pgRouting'"
 
 docker run -p 5050:5050 -e 'PGADMIN_DEFAULT_EMAIL=farfocla@gmail.com' -e 'PGADMIN_DEFAULT_PASSWORD=letMEin!' -d dpage/pgadmin4