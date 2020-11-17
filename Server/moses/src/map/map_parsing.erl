-module(map_parsing).

-export([road_id/1, junction_id/1, road_spec/1, junction_spec/1]).

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