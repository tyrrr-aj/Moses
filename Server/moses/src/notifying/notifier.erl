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
%%  road_network_element_type => road | junction
%%  road_network_element_id => RoadId,
%%  ride_id => RideId,
%%  begining_at => PartOfRoad,
%%  ending_at => PartOfRoad,
%%  direction => forward | backward,
%%  notification_code => 0 - "make way on road" | 1 - "make way on junction" | 2 - "no action required"
%% }]

%% marshalled notification:
%% <<"0 beg_at end_at road_id; text">>
%% <<"1 junction_id; text">>


%% API

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


notify(Notification) ->
    gen_server:cast(?SERVER, {notify, Notification}).


%% callbacks

init([]) ->
    {ok, Connection} = setup_connection(),
    {ok, Channel} = get_channel(Connection),
    setup_exchange(Channel),
    {ok, {Connection, Channel}}.


handle_call(_, _, _) ->
    should_not_be_used.


handle_cast({notify, Notification}, {_, Channel} = State) ->
    io:format("[notifier] got notification to send: ~p~n", [Notification]),
    Payload = marshall_notification(Notification),
    Publish = #'basic.publish'{exchange = exchange_name(), routing_key = choose_routing_key(Notification)},
    % io:format("[notifier] notification prepared ~n", []),
    amqp_channel:cast(Channel, Publish, #amqp_msg{payload = Payload}),
    % io:format("[notifier] notification sent ~n", []),
    {noreply, State}.


terminate(_, {Connection, Channel}) ->
    close_channel(Channel),
    close_connection(Connection),
    ok.


%% internal functions

setup_connection() ->
    amqp_connection:start(#amqp_params_network{}).


get_channel(Connection) ->
    amqp_connection:open_channel(Connection).


setup_exchange(Channel) ->
    Declare = #'exchange.declare'{exchange = exchange_name()},
    #'exchange.declare_ok'{} = amqp_channel:call(Channel, Declare).


marshall_notification(#{road_network_element_type := road, ride_id := RideId, begining_at := Begin, ending_at := End, direction := Direction, notification_body := Text}) ->
    DirectionRepr = case Direction of
                        backward -> 0;
                        forward -> 1
                    end,
    list_to_binary([0, trunc(Begin * 100), trunc(End * 100), DirectionRepr, list_to_binary(RideId), <<";">>, Text]);

marshall_notification(#{road_network_element_type := junction, ride_id := RideId, notification_body := Text}) ->
    list_to_binary([1, list_to_binary(RideId), <<";">>, Text]).



choose_routing_key(#{road_network_element_id := RoadNetworkElementId}) ->
    atom_to_binary(RoadNetworkElementId, routing_key_encoding()).


close_channel(Channel) ->
    amqp_channel:close(Channel).


close_connection(Connection) ->
    amqp_connection:close(Connection).


%% internal constants

exchange_name() -> <<"moses_exchange">>.


routing_key_encoding() ->
    latin1.


encoding() -> latin1.