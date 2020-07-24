-module(road_clearance).

-export([generate_vehicle_notifications/3, generate_junction_info/2]).


%% Notifications = [#{
%%  road_id => RoadId,
%%  begin => PartOfRoad,
%%  end => PartOfRoad,
%%  direction => forward | backward,
%%  notification_body => NotificationBody
%% }]

generate_vehicle_notifications(RoadSpec, EVs, PreviousNotifications) ->
    unimplemented.

generate_junction_info(RoadSpec, EVs) ->
    unimplemented.