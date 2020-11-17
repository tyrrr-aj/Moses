-module(manual_tracking).

-behaviour(gen_server).

-export([start_tracking/1, get_current_position/2]).
-export([update_position/2, end_ride/1]).
-export([init/1, handle_call/3, handle_cast/2]).

-define(SERVER, ?MODULE).

%% dispatch API

start_tracking(#{ride_id := RideId}) ->
    TrackingPid = case whereis(?SERVER) of
        undefined -> 
            {ok, Pid} = gen_server:start_link({local, ?SERVER}, ?MODULE, [], []),
            Pid;
        Pid -> Pid
        end,
    Position = gen_server:call(TrackingPid, {add_ride, RideId}, infinity),
    {ok, init_tracking_info(RideId, Position, TrackingPid)}.

get_current_position(RideId, TrackingPid) ->
    Position = gen_server:call(TrackingPid, {get_position, RideId}),
    {ok, {manual, Position}}.

end_ride(RideId) ->
    gen_server:cast(?SERVER, {end_ride, RideId}).


%% User interface

update_position(RideId, NewPosition) ->
    gen_server:cast(?SERVER, {update_position, RideId, NewPosition}).


%% Callbacks

init([]) ->
    {ok, #{}}.

handle_call({get_position, RideId}, _From, Positions) ->
    {reply, get_position(RideId, Positions), Positions};

handle_call({add_ride, RideId}, _From, Positions) ->
    {Position, UpdatedPositions} = add_ride(RideId, Positions),
    {reply, Position, UpdatedPositions}.

handle_cast({update_position, RideId, NewPosition}, Positions) ->
    {noreply, update_position(RideId, NewPosition, Positions)};

handle_cast({end_ride, RideId}, Positions) ->
    {noreply, maps:remove(RideId, Positions)}.


%% Internal functions

add_ride(RideId, Positions) ->
    PromptText = io_lib:format("Enter initial position for ride ~p (format: {road_id, part_of_road}): ", [RideId]),
    {Road, PartOfRoad} = prompt(PromptText),
    Position = {road, Road, PartOfRoad},
    {Position, Positions#{RideId => Position}}.

get_position(RideId, Positions) ->
    maps:get(RideId, Positions).

update_position(RideId, NewPosition, Positions) ->
    Positions#{RideId := NewPosition}.

prompt(Text) ->
    {ok, Input} = io:read(Text),
    Input.

init_tracking_info(RideId, Position, TrackingPid) ->
    #{
        ride_id => RideId,
        current_position => Position,
        previous_position => unknown,
        direction => not_moving,
        emergency_service_type =>  manual,
        connection => TrackingPid
    }.