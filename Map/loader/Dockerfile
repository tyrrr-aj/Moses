FROM postgres:latest

RUN apt-get update \
 && apt-get -y install apt-utils \
 && apt-get -y install expat \
 && apt-get -y install libexpat1-dev \
 && apt-get -y install libboost-dev \
 && apt-get -y install libboost-program-options-dev \
 && apt -y install libpqxx-dev \
 && apt-get -y install git \
 && apt-get -y install build-essential \
 && apt-get -y install cmake \
 && apt-get -y install net-tools

RUN git clone https://github.com/pgRouting/osm2pgrouting.git \
 && cd osm2pgrouting \
 && cmake -H. -Bbuild \
 && cd build \
 && make \
 && make install

RUN mkdir /data conf
COPY ./conf/* /conf


CMD osm2pgrouting --f /data/* --conf /conf/mapconfig.xml --dbname moses --username moses --password letMEin! --clean

# run with: docker run -v <path to dir containing single .osm file>:/data --network="host" --name mosesdb_loader mosesdb_loader:latest