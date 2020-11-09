create or replace function getRoadsInBox(
    x_lower_left IN numeric,
    y_lower_left IN numeric,
    x_upper_right IN numeric,
    y_upper_right IN numeric
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
                osm_id as "roadId",
                length_m as "length",
                source_osm as "origin",
                target_osm as "destination"
            from ways
            where ST_Intersects(
                the_geom,
                st_makepolygon(st_makeline(
                    ARRAY[
                        ST_SetSRID(ST_Point(x_lower_left, y_lower_left), 4326),
                        ST_SetSRID(ST_Point(x_upper_right, y_upper_right), 4326),
                        ST_SetSRID(ST_Point(x_upper_right, y_upper_right), 4326),
                        ST_SetSRID(ST_Point(x_lower_left, y_upper_right), 4326),
                        ST_SetSRID(ST_Point(x_lower_left, y_lower_left), 4326)
                        ]
                    ))
                );
    end;
    $$;