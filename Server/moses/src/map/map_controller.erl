-module(map_controller).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).
-export([get_position/1]).

-include_lib("epgsql/include/epgsql.hrl").


%% GPSCoords = #{lon := Lon, lat := Lat}


%% api

get_position(GPSCoords) ->
    gen_server:call(?MODULE, {get_position, GPSCoords}).


%% required interface

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% callbacks

init([]) ->
    Connection = map_connection:establish_connection(),
    {ok, Connection}.

handle_call({get_position, GPSCoords}, _, Connection) ->
    Position = map_connection:position_query(Connection, GPSCoords),
    {reply, Position, Connection}.

handle_cast(_, _) ->
    should_not_be_used.

terminate(_, Connection) ->
    map_connection:shutdown_connection(Connection),
    ok.

