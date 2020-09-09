-module(localization).

-export([get_position/1]).

%% position = {road, RoadId, PartOfRoad} | {junction, JunctionId}


%% test variants

get_position(mock_gps_coords) ->
    {road, mock_road, 0.6};

get_position({manual, {road, RoadId, PartOfRoad}}) ->
    {road, RoadId, PartOfRoad}.