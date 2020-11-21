-module(names).

-export([dispatch_module/1, dispatcher_name/1, tracking_module/1, tracker_name/1]).



dispatch_module(EmergencyServiceType) ->
    ServiceTypeAsList = atom_to_list(EmergencyServiceType),
    list_to_atom(ServiceTypeAsList ++ "_dispatch").


dispatcher_name(EmergencyServiceType) ->
    ServiceTypeAsList = atom_to_list(EmergencyServiceType),
    list_to_atom(ServiceTypeAsList ++ "_dispatcher").


tracking_module(EmergencyServiceType) ->
    ServiceType = atom_to_list(EmergencyServiceType),
    list_to_atom(ServiceType ++ "_tracking").


tracker_name(RideId) ->
    list_to_atom(RideId ++ "_tracker").