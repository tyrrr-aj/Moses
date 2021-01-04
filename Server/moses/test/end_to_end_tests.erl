-module(end_to_end_tests).

-include_lib("eunit/include/eunit.hrl").


single_message_pass_test() ->
    SetupData = setup(),

    test_receive_notification(SetupData),

    tear_down(SetupData).


%% setup

setup() ->
    moses_app:start(nothing, nothing),
    NotifierConnection = setup_map(),
    dispatch_single_ride(),
    NotifierConnection.

setup_map() ->
    NotifierConnection = setup_mock_road(),
    setup_mock_junctions(),
    NotifierConnection.

setup_mock_road() ->
    {ok, NotifierConnection} = notifier:setup_connection(),
    supervisor:start_child(road_controllers_sup, mock_road_controller_spec(NotifierConnection)),
    NotifierConnection.

setup_mock_junctions() ->
    supervisor:start_child(junction_controllers_sup, mock_junction_controller_spec(mock_junction_1)),
    supervisor:start_child(junction_controllers_sup, mock_junction_controller_spec(mock_junction_2)).

dispatch_single_ride() ->
    supervisor:start_child(dispatchers_sup, mock_dispatcher_spec()).


%% receiving notification

test_receive_notification(NotifierConnection) ->
    {ok, Channel} = notifier:get_channel(NotifierConnection),
    rabbitmq_con:prepare_test_queue(Channel, test_queue_name(), test_routing_key(), setup_timeout()),

    ReceivedMessage = rabbitmq_con:receive_single_notification(test_timeout()),
    ReferenceMessage = notifier:marshall_notification(reference_notification()),

    ?assertEqual(ReferenceMessage, ReceivedMessage),

    rabbitmq_con:delete_test_queue(Channel, test_queue_name()).

%% tear down

tear_down(NotifierConnection) ->
    notifier:close_connection(NotifierConnection),
    moses_app:stop(nothing).


%% constant specifications

setup_timeout() ->
    1000.

test_timeout() ->
    3000.

test_queue_name() ->
    <<"end_to_end_test_queue">>.

test_routing_key() ->
    notifier:choose_routing_key(#{road_id => mock_road}).

mock_dispatcher_spec() ->
    #{
        id => end_to_end_test_dispatcher,
        start => {dispatcher, start_link, [end_to_end_test]},
        restart => permanent,
        type => worker  
    }.

mock_road_controller_spec(NotifierConnection) ->
    #{
        id => mock_road_controller,
        start => {road_controller, start_link, [mock_road_spec(), NotifierConnection]},
        restart => permanent,
        type => worker    
    }.

mock_road_spec() ->
    #{
        id => mock_road,
        length => 100,
        beginning_at => mock_junction_1,
        ending_at => mock_junction_2
    }.

mock_junction_controller_spec(JunctionId) ->
    #{
        id => JunctionId,
        start => {junction_controller, start_link, [mock_junction_spec(JunctionId)]},
        restart => permanent,
        type => worker
    }.

mock_junction_spec(JunctionId) ->
    #{
        id => JunctionId,
        roads => [mock_road],
        type => plain
    }.

reference_notification() ->
    #{
        road_id => mock_road,
        begining_at => 0.0,
        ending_at => 1.0,
        direction => forward,
        notification_body => <<"unimplemented">>
    }.