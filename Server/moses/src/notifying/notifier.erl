-module(notifier).

-include_lib("amqp_client/include/amqp_client.hrl").

-export([setup_connection/0, get_channel/1, notify/2, close_channel/1, close_connection/1]).

-ifdef(EUNIT).
-compile(export_all).
-endif.

%% Notifications = [#{
%%  road_id => RoadId,
%%  begining_at => PartOfRoad,
%%  ending_at => PartOfRoad,
%%  direction => forward | backward,
%%  notification_body => <<"NotificationBody">>
%% }]

exchange_name() -> <<"moses_exchange">>.

routing_key_encoding() ->
    latin1.

%% API functions

setup_connection() ->
    {ok, Connection} = amqp_connection:start(#amqp_params_network{}),
    setup_exchange(Connection),
    {ok, Connection}.

get_channel(Connection) ->
    amqp_connection:open_channel(Connection).

notify(Notification, Channel) ->
    Payload = marshall_notification(Notification),
    Publish = #'basic.publish'{exchange = exchange_name(), routing_key = choose_routing_key(Notification)},
    amqp_channel:cast(Channel, Publish, #amqp_msg{payload = Payload}).

close_channel(Channel) ->
    amqp_channel:close(Channel).

close_connection(Connection) ->
    amqp_connection:close(Connection).

%% internal functions

setup_exchange(Connection) ->
    {ok, Channel} = get_channel(Connection),
    Declare = #'exchange.declare'{exchange = exchange_name()},
    #'exchange.declare_ok'{} = amqp_channel:call(Channel, Declare),
    close_channel(Channel).

marshall_notification(#{begining_at := Begin, ending_at := End, direction := Direction, notification_body := Text}) ->
    DirectionRepr = case Direction of
                        backward -> 0;
                        forward -> 1
                    end,
    list_to_binary([float_to_binary(Begin), <<";">>, float_to_binary(End), <<";">>, DirectionRepr, <<";">>, Text]).

choose_routing_key(#{road_id := RoadId}) ->
    atom_to_binary(RoadId, routing_key_encoding()).