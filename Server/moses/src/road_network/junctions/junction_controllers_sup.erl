-module(junction_controllers_sup).

-behaviour(supervisor).

-export([init/1]).
-export([start_link/1]).

start_link(JunctionSpecs) ->
    supervisor:start_link({local, junction_controllers_sup}, ?MODULE, JunctionSpecs).

init(JunctionSpecs) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [#{
                        id => Id,
                        start => {junction_controller, start_link, [Spec]},
                        restart => permanent,
                        type => worker
                    } || #{id := Id}=Spec <- JunctionSpecs
                ],
    {ok, {SupFlags, ChildSpecs}}. 
