-module(road_controller).

-behaviour(gen_server).

-export([handle_call/3, handle_cast/2, init/1]).
-export([start_link/1, add_ev/2, update_ev_position/2, remove_ev/2]).

%% RoadSpec = #{
%%  id -> RoadId,
%%  length -> Length,
%%  beginning -> JunctionId | dead_end,
%%  end -> JunctionId | dead_end
%% }

%% EVs = #{
%%  RideId -> #{
%%    part_of_road -> PartOfRoad,
%%    direction -> forward | backward,
%%    emergency_service_type -> EmergencyServiceType
%%  }
%% }]


%% interface

start_link(#{id := RoadId} = RoadSpec) ->
    gen_server:start_link({local, RoadId}, ?MODULE, [RoadSpec], []).

add_ev(RoadId, EvSpec) ->
    gen_server:cast(RoadId, {add_ev_impl, EvSpec}).

update_ev_position(RoadId, UpdateSpec) ->
    gen_server:cast(RoadId, {update_ev_impl, UpdateSpec}).

remove_ev(RoadId, RideId) ->
    gen_server:cast(RoadId, {remove_ev_impl, RideId}).


%% callbacks

init(#{id := RoadId} = RoadSpec) ->
    Connection = notifier:setup(RoadId),
    {ok, {RoadSpec, #{}, [], Connection}}.

handle_call(_, _, _) ->
    unimplemented.

handle_cast({Function, Args}, {RoadSpec, EVs, PreviousNotifications, Connection}) ->
    EVs = ?MODULE:Function(Args, EVs),
    NewNotifications = update_decisions(RoadSpec, EVs, PreviousNotifications, Connection),
    {ok, {RoadSpec, EVs, NewNotifications, Connection}}.

%% internal functions

add_ev_impl({RideId, PartOfRoad, Direction, EmergencyServiceType}, EVs) ->
    EVs#{RideId => #{part_of_road => PartOfRoad, direction => Direction, emergency_service_type => EmergencyServiceType}}.

update_ev_position_impl({RideId, PartOfRoad, Direction}, EVs) ->
    EVs#{RideId := #{part_of_road => PartOfRoad, direction => Direction}}.

remove_ev_impl(RideId, EVs) ->
    maps:remove(RideId, EVs).

update_decisions(RoadSpec, EVs, PreviousNotifications, Connection) ->
    notify_vehicles(RoadSpec, EVs, PreviousNotifications, Connection),
    inform_junctions(RoadSpec, EVs),
    ok.

notify_vehicles(RoadSpec, EVs, PreviousNotifications, Connection) ->
    Notifications = road_clearance:generate_vehicle_notifications(RoadSpec, EVs, PreviousNotifications),
    lists:foreach(fun(Notification) -> notifier:notify(Notification, Connection) end, Notifications),
    ok.

inform_junctions(RoadSpec, EVs) ->
    InfoForJunctions = road_clearance:generate_junction_info(RoadSpec, EVs),
    lists:foreach(fun(InfoForJunction) -> junction_controller:warn_of_incoming_evs(InfoForJunction) end, InfoForJunctions),
    ok.

    