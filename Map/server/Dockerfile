FROM pgrouting/pgrouting

RUN su postgres -c "psql -c 'CREATE DATABASE osm'" \
 && su postgres -c "psql --dbname osm -c 'CREATE EXTENSION postgis'" \
 && su postgres -c "psql --dbname osm -c 'CREATE EXTENSION pgRouting'"