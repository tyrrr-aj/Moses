-module(road_controller).

-behaviour(gen_server).

-export([handle_call/3, handle_cast/2, init/1, terminate/2]).
-export([start_link/2, add_ev/2, update_ev_position/2, remove_ev/2]).

%% RoadSpec = #{
%%  id -> RoadId,
%%  length -> Length,
%%  beginning_at -> JunctionId | dead_end,
%%  ending_at -> JunctionId | dead_end
%% }

%% EVs = #{
%%  RideId -> #{
%%    part_of_road -> PartOfRoad,
%%    direction -> forward | backward,
%%    emergency_service_type -> EmergencyServiceType
%%  }
%% }]


%% interface

start_link(#{id := RoadId} = RoadSpec, NotifierConnection) ->
    gen_server:start_link({local, RoadId}, ?MODULE, {RoadSpec, NotifierConnection}, []).

add_ev(RoadId, EvSpec) ->
    gen_server:cast(RoadId, {add_ev, EvSpec}).

update_ev_position(RoadId, UpdateSpec) ->
    gen_server:cast(RoadId, {update_ev, UpdateSpec}).

remove_ev(RoadId, RideId) ->
    gen_server:cast(RoadId, {remove_ev, RideId}).


%% callbacks

init({RoadSpec, NotifierConnection}) ->
    % io:format("[Road_controller] started~n", []),
    {ok, NotifierChannel} = notifier:get_channel(NotifierConnection),
    {ok, {RoadSpec, #{}, [], NotifierChannel}}.

handle_call(_, _, _) ->
    unimplemented.

handle_cast({add_ev, EVSpec}, {RoadSpec, EVs, PreviousNotifications, NotifierChannel}) ->
    % io:format("[Road_controller] position update received~n", []),
    NewEVs = add_to_evs(EVSpec, EVs),
    % io:format("[Road_controller] EVs map updated...~n", []),
    NewNotifications = update_decisions(RoadSpec, EVs, PreviousNotifications, NotifierChannel),
    % io:format("[Road_controller] position update processed succesfully!~n", []),
    {noreply, {RoadSpec, NewEVs, NewNotifications, NotifierChannel}};

handle_cast({update_ev, UpdateSpec}, {RoadSpec, EVs, PreviousNotifications, NotifierChannel}) ->
    % io:format("[Road_controller] position update received~n", []),
    NewEVs = update_ev_position_in_evs(UpdateSpec, EVs),
    NewNotifications = update_decisions(RoadSpec, EVs, PreviousNotifications, NotifierChannel),
    {noreply, {RoadSpec, NewEVs, NewNotifications, NotifierChannel}};

handle_cast({remove_ev, RideId}, {RoadSpec, EVs, PreviousNotifications, NotifierChannel}) ->
    % io:format("[Road_controller] position update received~n", []),
    NewEVs = remove_from_evs(RideId, EVs),
    NewNotifications = update_decisions(RoadSpec, EVs, PreviousNotifications, NotifierChannel),
    {noreply, {RoadSpec, NewEVs, NewNotifications, NotifierChannel}}.

terminate(_Reason, {_, _, _, NotifierChannel}) ->
    notifier:close_channel(NotifierChannel),
    ok.

%% internal functions

add_to_evs({RideId, PartOfRoad, Direction, EmergencyServiceType}, EVs) ->
    EVs#{RideId => #{part_of_road => PartOfRoad, direction => Direction, emergency_service_type => EmergencyServiceType}}.

update_ev_position_in_evs({RideId, PartOfRoad, Direction}, EVs) ->
    EVs#{RideId := #{part_of_road => PartOfRoad, direction => Direction}}.

remove_from_evs(RideId, EVs) ->
    maps:remove(RideId, EVs).

update_decisions(RoadSpec, EVs, PreviousNotifications, NotifierInfo) ->
    io:format("[Road controller] update started~n", []),
    notify_vehicles(RoadSpec, EVs, PreviousNotifications, NotifierInfo),
    io:format("[Road controller] Vehicles notified~n", []),
    inform_junctions(RoadSpec, EVs),
    io:format("[Road controller] Junctions informed~n", []),
    ok.

notify_vehicles(RoadSpec, EVs, PreviousNotifications, NotifierChannel) ->
    io:format("[Road controller] (notify_vehicles) started~n", []),
    Notifications = road_clearance:generate_vehicle_notifications(RoadSpec, EVs, PreviousNotifications),
    io:format("[Road controller] (notify_vehicles) notifications generated: ~p~n", [Notifications]),
    lists:foreach(fun(Notification) -> io:format("[Road controler] (notify_vehicles/lambda) notifier_channel: ~p, notification: ~p~n", [NotifierChannel, Notification]), notifier:notify(Notification, NotifierChannel) end, Notifications),
    io:format("[Road controller] (notify_vehicles) vehicles notified~n", []),
    ok.

inform_junctions(RoadSpec, EVs) ->
    InfoForJunctions = road_clearance:generate_junction_info(RoadSpec, EVs),
    lists:foreach(fun(InfoForJunction) -> junction_controller:warn_of_incoming_evs(InfoForJunction) end, InfoForJunctions),
    ok.

    