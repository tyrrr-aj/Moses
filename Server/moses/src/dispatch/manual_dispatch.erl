-module(manual_dispatch).

-behaviour(gen_server).

-export([establish_connection/0, get_new_rides/1]). % dispatch API
-export([add_ride/1]). % user interface
-export([init/1, handle_call/3, handle_cast/2]). % gen_server callbacks

-define(SERVER, ?MODULE).

%% dispatch API

establish_connection() ->
    DispatchPid = case whereis(?SERVER) of
        undefined -> 
            {ok, Pid} = gen_server:start_link({local, ?SERVER}, ?MODULE, [], []),
            Pid;
        Pid -> Pid
        end,
    {ok, DispatchPid}.

get_new_rides(DispatchPid) ->
    NewRides = gen_server:call(DispatchPid, get_rides),
    {ok, NewRides, DispatchPid}.


%% User interface

add_ride(RideData) ->
    gen_server:cast(?SERVER, {add_ride, ride_info(RideData)}).


%% Callbacks

init([]) ->
    {ok, []}.

handle_call(get_rides, _From, PendingRides) ->
    {reply, PendingRides, []}.

handle_cast({add_ride, NewRide}, PendingRides) ->
    {noreply, PendingRides ++ [NewRide]}.


%% Internal functions

ride_info(RideId) ->
    #{
        ride_id => RideId,
        emergency_service_type => manual
    }.