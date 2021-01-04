-module(junction_controller).

-behaviour(gen_server).

-export([handle_call/3, handle_cast/2, init/1]).
-export([start_link/1, warn_of_incoming_evs/1, pass_ev_coming_to_junction_special_info/1, add_ev/3, remove_ev/2]).

%% State = #{
%%    spec => #{
%%          id => JunctionId,
%%          roads => [RoadId],
%%          type => plain | traffic_lights,
%%          controller => TrafficLightsController
%%      },
%%
%%    evs => #{
%%          RideId => EmergencyServiceType
%%      }
%% }

%% EVsInfo = #{
%%  junction_id => JunctionId,
%%  road => RoadId,
%%  evs => #{
%%         RideId => #{
%%             remaining_distance => RemainingDistance,
%%             emergency_service_type => EmergencyServiceType
%%         }
%%     }
%% }


%% interface

start_link(#{id := JunctionId} = JunctionSpec) ->
    gen_server:start_link({local, JunctionId}, ?MODULE, JunctionSpec, []).


add_ev(JunctionId, RideId, EmergencyServiceType) ->
    gen_server:cast(JunctionId, {add_ev, RideId, EmergencyServiceType}).


remove_ev(JunctionId, RideId) ->
    gen_server:cast(JunctionId, {remove_ev, RideId}).


warn_of_incoming_evs(#{evs := EVs}) when map_size(EVs) == 0 -> ok;

warn_of_incoming_evs(#{junction_id := JunctionId, road_id := RoadId, evs := EVs}) ->
    % io:format("~p warn of evs by ~p~n", [JunctionId, RoadId]),
    gen_server:cast(JunctionId, {got_ev_warnings, RoadId, EVs}).


pass_ev_coming_to_junction_special_info(#{evs := EVs}) when map_size(EVs) == 0 -> ok;

pass_ev_coming_to_junction_special_info(#{junction_id := JunctionId, road_id := RoadId, evs := EVs}) ->
    % io:format("~p received info about evs coming to junction from ~p~n", [JunctionId, RoadId]),
    gen_server:cast(JunctionId, {passed_special_ev_warnings, RoadId, EVs}).


%% callbacks

init(JunctionSpec) ->
    case maps:get(type, JunctionSpec) of
        traffic_lights -> 
            Controller = maps:get(controller, JunctionSpec),
            Connected = Controller:setup(maps:get(id, JunctionSpec)),
            {ok, {JunctionSpec#{controller := Connected}, #{}, #{}}};
        _ -> {ok, #{spec => JunctionSpec, evs => #{}}}
    end.


handle_call(_, _, _) ->
    not_used.


handle_cast({add_ev, RideId, EmergencyServiceType}, #{evs := OldEVs} = State) ->
    NewState = State#{evs := OldEVs#{RideId => EmergencyServiceType}},
    notify_vehicles(NewState),
    warn_roads(NewState),
    {noreply, NewState};

handle_cast({remove_ev, RideId}, #{evs := OldEVs} = State) ->
    NewState = State#{evs := maps:remove(RideId, OldEVs)},
    {noreply, NewState};

handle_cast({got_ev_warnings, RoadId, EVs}, #{spec := JunctionSpec} = State) ->
    % io:format("Junction warn of evs: ~p~n", [EVs]),
    % notify_vehicles(),     % maybe vehicles shouldn't be notified in this situation?
    Warnings = junction_clearance:prepare_warnings_to_pass(JunctionSpec, RoadId, EVs),
    pass_warnings(State, Warnings),
    {noreply, State};

handle_cast({passed_special_ev_warnings, RoadId, EVs}, #{spec := JunctionSpec} = State) ->
    Warnings = junction_clearance:prepare_warnings_to_pass(JunctionSpec, RoadId, EVs),
    pass_warnings(State, Warnings, special),
    {noreply, State}.


%% internal functions

notify_vehicles(#{spec := #{id := JunctionId} = JunctionSpec, evs := EVs}) ->
    case is_internal_road_node(JunctionSpec) of
        true -> ok;
        false ->
            Notifications = junction_clearance:generate_notifications(JunctionId, EVs),
            lists:foreach(fun(Notification) -> notifier:notify(Notification) end, Notifications)
    end.


warn_roads(#{spec := #{id := JunctionId} = JunctionSpec} = State) ->
    % io:format("State: ~p~n", [State]),
    case is_internal_road_node(JunctionSpec) of
        true -> ok;
        false -> 
            Warnings = junction_clearance:generate_warnings(State),
            lists:foreach(fun({RoadId, EvInfo}) -> road_controller:warn_of_incoming_evs(RoadId, JunctionId, EvInfo) end, Warnings)
    end.


pass_warnings(#{spec := #{id := JunctionId} = JunctionSpec}, Warnings) ->
    % io:format("~p passes warnings: ~p...", [JunctionId, Warnings]),
    case is_internal_road_node(JunctionSpec) of
        true -> 
            % io:format(" along single road~n", []),
            [{RoadId, EvInfo}] = Warnings,
            road_controller:pass_ev_info_along_single_road(RoadId, JunctionId, EvInfo);
        false -> 
            % io:format(" to multiple roads~n", []),
            lists:foreach(fun({RoadId, EvInfo}) -> road_controller:warn_of_incoming_evs(RoadId, JunctionId, EvInfo) end, Warnings)
    end.


pass_warnings(#{spec := #{id := JunctionId}}, Warnings, special) ->
    % io:format("~p passes warnings: ~p...", [JunctionId, Warnings]),
    % io:format(" to multiple roads~n", []),
    lists:foreach(fun({RoadId, EvInfo}) -> road_controller:warn_of_incoming_evs(RoadId, JunctionId, EvInfo) end, Warnings).


is_internal_road_node(#{roads := [_, _]}) -> true;

is_internal_road_node(_) -> false.