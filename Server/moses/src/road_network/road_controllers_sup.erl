-module(road_controllers_sup).

-behaviour(supervisor).

-export([init/1]).
-export([start_link/1]).

start_link(RoadSpecs) ->
    io:format("Reached point 1~n", []),
    supervisor:start_link({local, road_controllers_sup}, ?MODULE, [RoadSpecs]).

init(RoadSpecs) ->
    io:format("Reached point 1.5~n", []),
    SupFlags = #{strategy => one_for_one,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [
                    #{
                        id => Id,
                        start => {road_controller, start_link, [Spec]},
                        restart => permanent,
                        type => worker
                    } || {Id, _}=Spec <- RoadSpecs
                ],
    io:format("Reached point 2~n", []),
    {ok, {SupFlags, ChildSpecs}}. 
