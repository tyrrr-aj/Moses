-module(junction_controllers_sup).

-behaviour(supervisor).

-export([init/1]).
-export([start_link/1]).

start_link(JunctionSepcs) ->
    supervisor:start_link({local, traffic_lights_controllers_sup}, ?MODULE, [JunctionSepcs]).

init(JunctionSepcs) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [#{
                        id => Id,
                        start => {traffic_lights_controller, start_link, [Spec]},
                        restart => permanent,
                        type => worker
                    } || {Id, _}=Spec <- JunctionSepcs
                ],
    {ok, {SupFlags, ChildSpecs}}. 
