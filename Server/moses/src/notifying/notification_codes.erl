-module(notification_codes).

-export([make_way_on_road_code/0, make_way_on_junction_code/0, action_unnecessary_code/0]).


make_way_on_road_code() -> 0.

make_way_on_junction_code() -> 1.

action_unnecessary_code() -> 2. 
