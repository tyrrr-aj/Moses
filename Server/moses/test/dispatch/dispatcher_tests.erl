-module(dispatcher_tests).

-include_lib("eunit/include/eunit.hrl").


%% tests

% check_for_new_rides_test() ->
%     NewRides = test_dispatch:check_for_new_rides()



%% internal functions

setup() ->
    {ok, DispatcherSup} = dispatchers_sup:start_link(),
    supervisor:start_child(DispatcherSup, mock_dispatcher_spec()).



%% constants

mock_dispatcher_spec() ->
    #{
        id => test_dispatcher,
        start => {dispatcher, start_link, [test]},
        restart => permanent,
        type => worker
    }.