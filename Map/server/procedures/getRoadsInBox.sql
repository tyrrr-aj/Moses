create or replace function getRoadsInBox(
    x_lower_left IN double precision,
    y_lower_left IN double precision,
    x_upper_right IN double precision,
    y_upper_right IN double precision
)
returns table(
    roadId bigint,
    length double precision,
    origin bigint,
    destination bigint
             )
language plpgsql
as
    $$
    begin
        return query
            select
                gid as "roadId",
                length_m as "length",
                source as "origin",
                target as "destination"
            from ways
            where ST_Intersects(
                the_geom,
                st_makepolygon(st_makeline(
                    ARRAY[
                        ST_SetSRID(ST_Point(x_lower_left, y_lower_left), 4326),
                        ST_SetSRID(ST_Point(x_upper_right, y_lower_left), 4326),
                        ST_SetSRID(ST_Point(x_upper_right, y_upper_right), 4326),
                        ST_SetSRID(ST_Point(x_lower_left, y_upper_right), 4326),
                        ST_SetSRID(ST_Point(x_lower_left, y_lower_left), 4326)
                        ]
                    ))
                );
    end;
    $$;