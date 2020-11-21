-module(map_controller).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([get_position/1]).

-include_lib("epgsql/include/epgsql.hrl").


%% GPSCoords = #{lon := Lon, lat := Lat}


%% api

get_position(GPSCoords) ->
    ok = gen_server:call(?MODULE, {get_position, GPSCoords}),
    receive
        {ok, Position} -> Position
    end.

%% required interface

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% callbacks

init([]) ->
    ConnectionDetails = map_connection:establish_connection(),
    {ok, #{connection => ConnectionDetails, requests => #{}}}.


handle_call({get_position, GPSCoords}, {From, _Tag}, #{connection := ConnectionDetails, requests := Requests} = State) ->
    Ref = map_connection:position_query(ConnectionDetails, GPSCoords),
    UpdatedRequests = Requests#{Ref => From},
    {reply, ok, State#{requests := UpdatedRequests}}.


handle_cast(_, _) ->
    should_not_be_used.


handle_info({_Connection, Ref, {ok, _ColumnNames, Result}}, #{requests := Requests} = State) ->
    Position = map_connection:parse_position_query_result(Result),
    RespondTo = maps:get(Ref, Requests),
    UpdatedRequests = maps:remove(Ref, Requests),
    RespondTo ! {ok, Position},
    {noreply, State#{requests := UpdatedRequests}}.


terminate(_, #{connection := ConnectionDetails}) ->
    map_connection:shutdown_connection(ConnectionDetails),
    ok.
