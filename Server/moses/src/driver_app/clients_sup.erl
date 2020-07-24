-module(clients_sup).

-behaviour(supervisor).

-export([init/1]).
-export([start_link/0, add_client/1, remove_client/1]).


%% API

start_link() ->
    supervisor:start_link({local, clients_sup}, ?MODULE, []).

add_client(ClientInfo) ->
    supervisor:start_child(?MODULE, ClientInfo).

remove_client(Pid) ->
    supervisor:terminate_child(?MODULE, Pid).


%% Callbacks

init([]) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [#{
                        id => client_guardian,
                        start => {client_guardian, start_link, []},
                        restart => transient,
                        type => worker
                    }
                ],
    {ok, {SupFlags, ChildSpecs}}. 
