-module(road_controller).

-behaviour(gen_server).

-export([handle_call/3, handle_cast/2, init/1]).
-export([start_link/1, add_ev/2, update_ev_position/2, remove_ev/2]).
-export([warn_of_incoming_evs/3, pass_ev_info_along_single_road/3]).

%% RoadSpec = #{
%%  id => RoadId,
%%  length => Length,
%%  beginning_at => JunctionId | dead_end,
%%  ending_at => JunctionId | dead_end
%% }

%% EVs = #{
%%  RideId => #{
%%    part_of_road => PartOfRoad,
%%    direction => forward | backward,
%%    emergency_service_type => EmergencyServiceType
%%  }
%% }]


%% interface

start_link(#{id := RoadId} = RoadSpec) ->
    gen_server:start_link({local, RoadId}, ?MODULE, {RoadSpec}, []).


add_ev(RoadId, EvSpec) ->
    gen_server:cast(RoadId, {add_ev, EvSpec}).


update_ev_position(RoadId, EvSpec) ->
    gen_server:cast(RoadId, {update_ev, EvSpec}).


remove_ev(RoadId, RideId) ->
    gen_server:cast(RoadId, {remove_ev, RideId}).


% warn_of_incoming_evs(_, _, []) -> ok;

warn_of_incoming_evs(RoadId, JunctionId, EvInfo) ->
    % io:format("Road warn of incoming EVs: ~p~n", [EvInfo]),
    gen_server:cast(RoadId, {incoming, JunctionId, EvInfo}).


pass_ev_info_along_single_road(RoadId, JunctionId, EvInfo) ->
    % io:format("EVs passed to road: ~p~n", [EvInfo]),
    gen_server:cast(RoadId, {pass, JunctionId, EvInfo}).


%% callbacks

init({RoadSpec}) ->
    % io:format("[Road_controller] started~n", []),
    {ok, {RoadSpec, #{}, []}}.


handle_call(_, _, _) ->
    should_not_be_used.


handle_cast({add_ev, EVSpec}, {RoadSpec, EVs, PreviousNotifications}) ->
    % io:format("[Road_controller] position update received~n", []),
    print_ev_position(RoadSpec, EVSpec),
    NewEVs = add_to_evs(EVSpec, EVs),
    % io:format("[Road_controller] EVs map updated...~n", []),
    NewNotifications = update_decisions(RoadSpec, NewEVs, PreviousNotifications, original),
    % io:format("Ev added on road ~s~n", [maps:get(id, RoadSpec)]),
    % io:format("[Road_controller] position update processed succesfully!~n", []),
    {noreply, {RoadSpec, NewEVs, NewNotifications}};

handle_cast({update_ev, EVSpec}, {RoadSpec, EVs, PreviousNotifications}) ->
    % io:format("[Road_controller] position update received~n", []),
    print_ev_position(RoadSpec, EVSpec),
    NewEVs = update_ev_position_in_evs(EVSpec, EVs),
    NewNotifications = update_decisions(RoadSpec, NewEVs, PreviousNotifications, original),
    {noreply, {RoadSpec, NewEVs, NewNotifications}};

handle_cast({remove_ev, RideId}, {RoadSpec, EVs, PreviousNotifications}) ->
    % io:format("[Road_controller] position update received~n", []),
    NewEVs = remove_from_evs(RideId, EVs),
    NewNotifications = update_decisions(RoadSpec, NewEVs, PreviousNotifications, original),
    {noreply, {RoadSpec, NewEVs, NewNotifications}};

handle_cast({incoming, JunctionId, EvInfo}, {RoadSpec, EVs, PreviousNotifications}) ->
    io:format("~p got info of incoming evs: ~p~n", [maps:get(id, RoadSpec), EvInfo]),
    Notifications = road_clearance:generate_notifications_for_vehicles_coming_to_junction(RoadSpec, JunctionId, EvInfo),
    io:format("~p generated notifications: ~p~n", [maps:get(id, RoadSpec), Notifications]),
    lists:foreach(fun(Notification) -> notifier:notify(Notification) end, Notifications),
    SpecialWarnings = road_clearance:prepare_info_from_junction_to_pass(RoadSpec, JunctionId, EvInfo),
    io:format("~p passes special warnings: ~p~n", [maps:get(id, RoadSpec), SpecialWarnings]),
    pass_special_info_to_junctions(SpecialWarnings),
    {noreply, {RoadSpec, EVs, PreviousNotifications}};

handle_cast({pass, JunctionId, EvInfo}, {RoadSpec, EVs, PreviousNotifications}) ->
    PassedEvs = road_clearance:retrieve_passed_evs(RoadSpec, JunctionId, EvInfo),
    NewNotifications = update_decisions(RoadSpec, PassedEvs, PreviousNotifications, passed),
    {noreply, {RoadSpec, EVs, NewNotifications}}.



%% internal functions

add_to_evs({RideId, PartOfRoad, Direction, EmergencyServiceType}, EVs) ->
    EVs#{RideId => #{part_of_road => PartOfRoad, direction => Direction, emergency_service_type => EmergencyServiceType}}.


update_ev_position_in_evs({RideId, PartOfRoad, Direction, _} = EVSpec, EVs) ->
    case EVs of
        #{ride_id := _} -> EVs#{RideId := #{part_of_road => PartOfRoad, direction => Direction}};
        _ -> add_to_evs(EVSpec, EVs)
    end.   


remove_from_evs(RideId, EVs) ->
    maps:remove(RideId, EVs).


update_decisions(RoadSpec, EVs, PreviousNotifications, InfoSource) ->
    % io:format("[Road controller] update started~n", []),
    notify_vehicles(RoadSpec, EVs, PreviousNotifications),
    % io:format("[Road controller] Vehicles notified~n", []),
    inform_junctions(RoadSpec, EVs, InfoSource),
    % io:format("[Road controller] Junctions informed~n", []),
    ok.


notify_vehicles(RoadSpec, EVs, _PreviousNotifications) ->
    % io:format("[Road controller] (notify_vehicles) started~n", []),
    Notifications = road_clearance:generate_vehicle_notifications(RoadSpec, EVs),
    io:format("~p generated notifications: ~p~n", [maps:get(id, RoadSpec), Notifications]),
    lists:foreach(fun(Notification) -> notifier:notify(Notification) end, Notifications),
    % lists:foreach(fun(Notification) -> io:format("[Road controler] (notify_vehicles/lambda) notification: ~p~n", [Notification]), notifier:notify(Notification) end, Notifications),
    % io:format("[Road controller] (notify_vehicles) vehicles notified~n", []),
    ok.


inform_junctions(RoadSpec, EVs, InfoSource) ->
    case InfoSource of
        original -> InfoForJunctions = road_clearance:generate_info_for_junctions(RoadSpec, EVs);
        passed -> InfoForJunctions = road_clearance:pass_info_for_junctions(RoadSpec, EVs)
    end,
    % io:format("EVs: ~p~n Info for junctions: ~p~n", [EVs, InfoForJunctions]),
    lists:foreach(fun(InfoForJunction) -> junction_controller:warn_of_incoming_evs(InfoForJunction) end, InfoForJunctions).


pass_special_info_to_junctions(InfoForJunctions) ->
    lists:foreach(fun(InfoForJunction) -> junction_controller:pass_ev_coming_to_junction_special_info(InfoForJunction) end, InfoForJunctions).


print_ev_position(#{id := RoadId}, {_RideId, PartOfRoad, Direction, _EmergencyServiceType}) ->
    io:format("Position: {~p,~p,~p}~n", [RoadId, PartOfRoad, Direction]).
