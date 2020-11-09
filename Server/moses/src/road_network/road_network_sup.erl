%%%-------------------------------------------------------------------
%% @doc moses top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(road_network_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional

init([]) ->
    {RoadControllersSpecs, JunctionControllersSpecs} = load_network(),
    SupFlags = #{strategy => one_for_one,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [
                    #{
                        id => road_controllers_sup,
                        start => {road_controllers_sup, start_link, [RoadControllersSpecs]},
                        restart => permanent,
                        type => supervisor
                    },
                    #{
                        id => junction_controllers_sup,
                        start => {junction_controllers_sup, start_link, [JunctionControllersSpecs]},
                        restart => permanent,
                        type => supervisor
                    }
                ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions

%% RoadSpec = {road_id, length, origin_id, destination_id}
%% JunctionSpec = {junction_id, [road_id, ...]}

load_network() ->
    io:format("Loading network...~n", []),
    case file:read_file(cache_file_name()) of
        {error, _} ->   io:format("  cache not found, loding from database...~n", []),
                        {RoadSpecs, JunctionSpecs} = fetch_data(),
                        ControllersSpecs = {lists:map(fun(RoadSpec) -> road_controller_spec(RoadSpec) end, RoadSpecs), lists:map(fun(JunctionSpec) -> junction_controller_spec(JunctionSpec) end, JunctionSpecs)},
                        io:format("  caching loded data~n", []),
                        cache_network(ControllersSpecs),
                        io:format("network loaded~n", []),
                        ControllersSpecs;
        {ok, SerializedNetwork} ->  io:format("found cached network, loading~n", []),
                                    deserialize_network(SerializedNetwork)
    end.

fetch_data() ->
    Connection = map_connection:establish_connection(),
    io:format("    connection established...~n", []),
    Roads = map_connection:query_for_roads(Connection, lower_left_boundry(), upper_right_boundry()),
    io:format("    roads fetched...~n", []),
    Junctions = map_connection:query_for_junctions(Connection, lower_left_boundry(), upper_right_boundry()),
    io:format("    junctions fetched, shutting down connection~n", []),
    map_connection:shutdown_connection(Connection),
    {Roads, Junctions}.

road_controller_spec({RoadId, Length, OriginId, DestinationId}) ->
    #{
        id => RoadId,
        length => Length,
        beginning_at => OriginId,
        ending_at => DestinationId
    }.

junction_controller_spec({JunctionId, Roads}) ->
    #{
        id => JunctionId,
        roads => Roads,
        type => plain
    }.

deserialize_network(Serialized) ->
    binary_to_term(Serialized).

cache_network(ControllersSpecs) ->
    Serialized = term_to_binary(ControllersSpecs),
    file:write_file(cache_file_name(), Serialized).
    


%% internal constants

cache_file_name() -> "road_network.data".

lower_left_boundry() -> #{lon => 19.8255, lat => 50.0402}.

upper_right_boundry() -> #{lon => 20.0504, lat => 50.0886}.