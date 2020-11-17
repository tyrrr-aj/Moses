-module(notifier_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    SupervisorSpecification = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60},

    ChildSpecifications = [
        #{
            id => notifier,
            start => {notifier, start_link, []},
            restart => permanent,
            shutdown => 2000,
            type => worker,
            modules => [notifier]
        }
    ],

    {ok, {SupervisorSpecification, ChildSpecifications}}.