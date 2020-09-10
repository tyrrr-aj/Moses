%%%-------------------------------------------------------------------
%% @doc moses top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(moses_sup).

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
                        id => dispatchers_supervisor,
                        start => {dispatchers_sup, start_link, []},
                        restart => permanent,
                        type => supervisor
                    },
                    #{
                        id => ev_trackers_supervisor,
                        start => {ev_trackers_sup, start_link, []},
                        restart => permanent,
                        type => supervisor
                    },
                    #{
                        id => road_network_sup,
                        start => {road_network_sup, start_link, []},
                        restart => permanent,
                        type => supervisor
                    }
                    % #{
                    %     id => driver_apps_controller,
                    %     start => {driver_apps_controller, start_link, []},
                    %     restart => permanent,
                    %     type => worker
                    % }
                ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
