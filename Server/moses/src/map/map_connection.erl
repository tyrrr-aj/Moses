-module(map_connection).

-export([get_position/1]).
-export([establish_connection/0, position_query/2, query_for_roads/3, query_for_junctions/3, parse_position_query_result/1, shutdown_connection/1]).

%% position = {road, RoadId, PartOfRoad} | {junction, JunctionId} | unknown


%% API

establish_connection() ->
    {ok, Connection} = epgsqla:start_link(),
    Ref = epgsqla:connect(Connection, <<"localhost">>, <<"moses">>, <<"Split now!">>, #{database => <<"osm">>}),
    receive
        {Connection, Ref, connected} ->
            Connection;
        {Connection, Ref, Error} ->
            Error;
        {'EXIT', Connection, _Reason} ->
            {error, closed}
    end.

position_query(Connection, #{lon := Lon, lat := Lat}) ->
    % io:format("Lon: ~f, lat: ~f~n", [Lon, Lat]),
    Ref = epgsqla:equery(Connection, "select * from findNearestRoad($1, $2)", [Lon, Lat]),
    Ref.

shutdown_connection(Connection) ->
    ok = epgsql:close(Connection).

query_for_roads(Connection, #{lon := LonLowerLeft, lat := LatLowerLeft}, #{lon := LonUpperRight, lat := LatUpperRight}) ->
    {ok, _, Rows} = epgsql:equery(Connection, "select * from getRoadsInBox($1, $2, $3, $4)", [LonLowerLeft, LatLowerLeft, LonUpperRight, LatUpperRight]),
    lists:map(fun(Row) -> map_parsing:road_spec(Row) end, Rows).

query_for_junctions(Connection, #{lon := LonLowerLeft, lat := LatLowerLeft}, #{lon := LonUpperRight, lat := LatUpperRight}) ->
    {ok, _, Rows} = epgsql:equery(Connection, "select * from getJunctionsInBox($1, $2, $3, $4)", [LonLowerLeft, LatLowerLeft, LonUpperRight, LatUpperRight]),
    lists:map(fun(Row) -> map_parsing:junction_spec(Row) end, Rows).


parse_position_query_result(Rows) ->
    case single_row(Rows) of
        no_rows -> unknown;
        {RoadNumber, true, PartOfRoad} -> {road, map_parsing:road_id(RoadNumber), PartOfRoad};
        {JunctionNumber, false, _} -> {junction, map_parsing:junction_id(JunctionNumber)}
    end.


%% internal_functions

single_row([]) ->
    no_rows;

single_row([Row]) ->
    Row.


%% internal_constants

% connection_parameters() ->
%     #{
%         host => "localhost",
%         username => "moses",
%         password => "Split now!",
%         database => "osm"
%     }.


%% test variants

get_position(mock_gps_coords) ->
    {road, mock_road, 0.6};

get_position({manual, {road, RoadId, PartOfRoad}}) ->
    {road, RoadId, PartOfRoad}.