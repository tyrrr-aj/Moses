create or replace function getJunctionsInBox(
    x_lower_left IN double precision,
    y_lower_left IN double precision,
    x_upper_right IN double precision,
    y_upper_right IN double precision
)
returns table(
    junctionId bigint,
    roads bigint[]
             )
language plpgsql
as
$$
    begin
        return query
        select
               junctions.id as "junctionId",
               array_agg(roads.gid) as "roads"
        from ways_vertices_pgr junctions
            join ways roads on junctions.id = roads.source or junctions.id = roads.target
        where ST_Contains(
             st_makepolygon(st_makeline(
                    ARRAY[
                        ST_SetSRID(ST_Point(x_lower_left, y_lower_left), 4326),
                        ST_SetSRID(ST_Point(x_upper_right, y_lower_left), 4326),
                        ST_SetSRID(ST_Point(x_upper_right, y_upper_right), 4326),
                        ST_SetSRID(ST_Point(x_lower_left, y_upper_right), 4326),
                        ST_SetSRID(ST_Point(x_lower_left, y_lower_left), 4326)
                        ]
                    )),
            junctions.the_geom
            )
        group by junctions.id;
    end;
$$;