-module(map_connection).

-export([get_position/1]).
-export([establish_connection/0, position_query/2, query_for_roads/3, query_for_junctions/3, parse_position_query_result/1, shutdown_connection/1]).

%% position = {road, RoadId, PartOfRoad} | {junction, JunctionId} | unknown


%% API

establish_connection() ->
    {ok, Connection} = epgsqla:start_link(),
    Ref = epgsqla:connect(Connection, "localhost", "moses", "letMEin!", #{database => "moses"}),
    {ok, Connection} = receive
        {Connection, Ref, connected} ->
            {ok, Connection};
        {Connection, Ref, Error} ->
            Error;
        {'EXIT', Connection, _Reason} ->
            {error, closed}
    end,
    {ok, PositionQueryStatement} = epgsql:parse(Connection, prepared_position_query_name(), "select * from findNearestRoad($1, $2)", position_query_parameter_types()),
    {Connection, PositionQueryStatement}.


position_query({Connection, PositionQueryStatement}, #{lon := Lon, lat := Lat}) ->
    % io:format("Lon: ~f, lat: ~f~n", [Lon, Lat]),
    TypedParameters = lists:zip(position_query_parameter_types(), [Lon, Lat]),
    Ref = epgsqla:prepared_query(Connection, PositionQueryStatement, TypedParameters),
    Ref.


shutdown_connection({Connection, _}) ->
    ok = epgsql:close(Connection).


query_for_roads({Connection, _}, #{lon := LonLowerLeft, lat := LatLowerLeft}, #{lon := LonUpperRight, lat := LatUpperRight}) ->
    {ok, _, Rows} = epgsql:equery(Connection, "select * from getRoadsInBox($1, $2, $3, $4)", [LonLowerLeft, LatLowerLeft, LonUpperRight, LatUpperRight]),
    lists:map(fun(Row) -> map_parsing:road_spec(Row) end, Rows).


query_for_junctions({Connection, _}, #{lon := LonLowerLeft, lat := LatLowerLeft}, #{lon := LonUpperRight, lat := LatUpperRight}) ->
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


prepared_position_query_name() -> "position_query".


position_query_parameter_types() -> [float8, float8].

% connection_parameters() ->
%     #{
%         host => "localhost",
%         username => "moses",
%         password => "letMEin!",
%         database => "moses"
%     }.


%% test variants

get_position(mock_gps_coords) ->
    {road, mock_road, 0.6};

get_position({manual, {road, RoadId, PartOfRoad}}) ->
    {road, RoadId, PartOfRoad}.