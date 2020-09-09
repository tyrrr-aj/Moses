-module(dispatchers_sup).

-behaviour(supervisor).

-export([start_link/0, start_manual/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional

start_manual() ->
    supervisor:start_child(?SERVER, #{
            id => manual_dispatcher,
            start => {dispatcher, start_link, [manual]},
            restart => permanent,
            type => worker
        }).

init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [
                    % #{
                    %     id => test,
                    %     start => {dispatcher, start_link, [test]},
                    %     restart => permanent,
                    %     type => worker
                    % }
                ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions

