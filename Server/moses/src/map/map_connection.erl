-module(map_connection).

-export([get_position/1]).
-export([establish_connection/0, position_query/2, query_for_roads/3, query_for_junctions/3, shutdown_connection/1]).

%% position = {road, RoadId, PartOfRoad} | {junction, JunctionId} | unknown


%% API

establish_connection() ->
    {ok, Connection} = epgsql:connect(connection_parameters()),
    Connection.

position_query(Connection, #{lon := Lon, lat := Lat}) ->
    io:format("Lon: ~f, lat: ~f~n", [Lon, Lat]),
    {ok, _, Rows} = epgsql:equery(Connection, "select * from findNearestRoad($1, $2)", [Lon, Lat]),
    case single_row(Rows) of
        no_rows -> unknown;
        {RoadNumber, PartOfRoad} -> {road_id(RoadNumber), PartOfRoad}
    end.

shutdown_connection(Connection) ->
    ok = epgsql:close(Connection).

query_for_roads(Connection, #{lon := LonLowerLeft, lat := LatLowerLeft}, #{lon := LonUpperRight, lat := LatUpperRight}) ->
    {ok, _, Rows} = epgsql:equery(Connection, "select * from getRoadsInBox($1, $2, $3, $4)", [LonLowerLeft, LatLowerLeft, LonUpperRight, LatUpperRight]),
    lists:map(fun(Row) -> road_spec(Row) end, Rows).

query_for_junctions(Connection, #{lon := LonLowerLeft, lat := LatLowerLeft}, #{lon := LonUpperRight, lat := LatUpperRight}) ->
    {ok, _, Rows} = epgsql:equery(Connection, "select * from getJunctionsInBox($1, $2, $3, $4)", [LonLowerLeft, LatLowerLeft, LonUpperRight, LatUpperRight]),
    lists:map(fun(Row) -> junction_spec(Row) end, Rows).

%% internal_functions

single_row([]) ->
    no_rows;

single_row([Row]) ->
    Row.

road_id(RoadNumber) ->
    list_to_atom(lists:flatten(io_lib:format("~s~B", [road, RoadNumber]))).

junction_id(null) ->
    dead_end;

junction_id(JunctionNumber) ->
    list_to_atom(lists:flatten(io_lib:format("~s~B", [junction, JunctionNumber]))).

road_spec({RoadNumber, Length, Origin, Destination}) ->
    {road_id(RoadNumber), Length, junction_id(Origin), junction_id(Destination)}.

junction_spec({JunctionNumber, RoadNumbers}) ->
    {junction_id(JunctionNumber), lists:map(fun(RoadNumber) -> road_id(RoadNumber) end, RoadNumbers)}.


%% internal_constants

connection_parameters() ->
    #{
        host => "localhost",
        username => "moses",
        password => "Split now!",
        database => "osm"
    }.


%% test variants

get_position(mock_gps_coords) ->
    {road, mock_road, 0.6};

get_position({manual, {road, RoadId, PartOfRoad}}) ->
    {road, RoadId, PartOfRoad}.