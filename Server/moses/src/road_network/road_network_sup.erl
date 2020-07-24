%%%-------------------------------------------------------------------
%% @doc moses top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(road_network_sup).

-behaviour(supervisor).

-export([start_link/0]).

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
init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [
                    #{
                        id => road_controllers_supervisor,
                        start => {road_controllers_sup, start_link, []},
                        restart => permanent,
                        type => supervisor
                    },
                    #{
                        id => traffic_lights_controllers_supervisor,
                        start => {traffic_lights_controllers_sup, start_link, []},
                        restart => permanent,
                        type => supervisor
                    }
                ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions