FROM pgrouting/pgrouting

ENV POSTGRES_USER=moses

COPY ./procedures/* /docker-entrypoint-initdb.d/


# run with: docker run -p 5432:5432 -e 'POSTGRES_PASSWORD=letMEin!' -d --name mosesdb mosesdb_server:latest