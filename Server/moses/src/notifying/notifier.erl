-module(notifier).
-behaviour(gen_server).

-include_lib("amqp_client/include/amqp_client.hrl").

-export([init/1, handle_call/3, handle_cast/2, terminate/2]).
-export([start_link/0]).
-export([setup_connection/0, notify/1]).

-ifdef(EUNIT).
-compile(export_all).
-endif.

-define(SERVER, ?MODULE).

%% Notifications = [#{
%%  road_id => RoadId,
%%  begining_at => PartOfRoad,
%%  ending_at => PartOfRoad,
%%  direction => forward | backward,
%%  notification_body => <<"NotificationBody">>
%% }]


%% API

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


notify(Notification) ->
    gen_server:cast({notify, Notification}, ?SERVER).


%% callbacks

init([]) ->
    {ok, Connection} = setup_connection(),
    {ok, Channel} = get_channel(Connection),
    setup_exchange(Channel),
    {ok, {Connection, Channel}}.


handle_call(_, _, _) ->
    should_not_be_used.


handle_cast({notify, Notification}, {_, Channel} = State) ->
    Payload = marshall_notification(Notification),
    Publish = #'basic.publish'{exchange = exchange_name(), routing_key = choose_routing_key(Notification)},
    amqp_channel:cast(Channel, Publish, #amqp_msg{payload = Payload}),
    {noreply, State}.


terminate(_, {Channel, Connection}) ->
    close_channel(Channel),
    close_connection(Connection),
    ok.


%% internal functions

setup_connection() ->
    {ok, Connection} = amqp_connection:start(#amqp_params_network{}),
    % setup_exchange(Connection),
    {ok, Connection}.

get_channel(Connection) ->
    amqp_connection:open_channel(Connection).

setup_exchange(Channel) ->
    Declare = #'exchange.declare'{exchange = exchange_name()},
    #'exchange.declare_ok'{} = amqp_channel:call(Channel, Declare).

marshall_notification(#{begining_at := Begin, ending_at := End, direction := Direction, notification_body := Text}) ->
    DirectionRepr = case Direction of
                        backward -> 0;
                        forward -> 1
                    end,
    list_to_binary([trunc(Begin * 100), trunc(End * 100), DirectionRepr, Text]).

choose_routing_key(#{road_id := RoadId}) ->
    atom_to_binary(RoadId, routing_key_encoding()).

close_channel(Channel) ->
    amqp_channel:close(Channel).

close_connection(Connection) ->
    amqp_connection:close(Connection).


%% internal constants

exchange_name() -> <<"moses_exchange">>.

routing_key_encoding() ->
    latin1.