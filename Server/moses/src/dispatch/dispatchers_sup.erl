-module(dispatchers_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    Response = supervisor:start_link({local, ?SERVER}, ?MODULE, []),
    case Response of
        {ok, _} ->
            supervisor:start_child(?SERVER, [firefighters_dispatch])
    end,
    Response.

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional

init([]) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [#{
                        id => test,
                        start => {dispatcher, start_link, [test]},
                        restart => permanent,
                        type => worker
                    }
                ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions

