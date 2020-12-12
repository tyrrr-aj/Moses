create or replace function isOnJunction(
    road_length IN double precision,
    part_of_road IN double precision,
    origin_id IN bigint,
    destination_id IN bigint
)
returns bigint
language plpgsql
as
    $$
    declare
        junction_radius constant numeric := 5.0; -- in meters
    begin
        if part_of_road < 0.5 then
            if part_of_road * road_length < junction_radius then
                return origin_id;
            end if;
        else
            if (1.0 - part_of_road) * road_length < junction_radius then
                return destination_id;
            end if;
        end if;
        return -1;
    end;
    $$;