#FROM pgrouting/pgrouting:latest
#RUN apt-get update \
# && apt-get -y install apt-utils \
# && apt-get -y install expat \
# && apt-get -y install libexpat1-dev \
# && apt-get -y install libboost-dev \
# && apt-get -y install libboost-program-options-dev \
# && apt -y install libpqxx-dev \
# && apt-get -y install git \
# && apt-get -y install build-essential \
# && apt-get -y install cmake \
# && apt-get -y install net-tools

#FROM pgrouting:basic
#
#RUN git clone https://github.com/pgRouting/osm2pgrouting.git \
# && cd osm2pgrouting \
# && cmake -H. -Bbuild \
# && cd build \
# && make \
# && make install

FROM pgrouting:installed

COPY ./bin .

RUN mkdir data
COPY ./data ./data

COPY ./conf .

ENTRYPOINT [ "/setup.sh" ]