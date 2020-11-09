create or replace function findNearestRoad(
    lon IN numeric,
    lat IN numeric
)
    returns table(
        "roadId" bigint,
        "partOfRoad" double precision
             )
    language plpgsql
as
    $$
    begin
        return query
                select
                    "Id",
                    ST_Distance("Closest", "Beginning", false) / "Length" AS "Distance"
                from (
                         select
                                osm_id as "Id",
                                ST_ClosestPoint(the_geom, ST_SetSRID(ST_Point(lon, lat), 4326)) AS "Closest",
                                                ST_SetSRID(ST_Point(x1, y1), 4326) AS "Beginning",
                                                "length_m" as "Length"
                        from ways
                        order by ST_Distance(the_geom, ST_SetSRID(ST_Point(lon, lat), 4326)::geography)
                        limit 1
                    ) closest_edge;
    end;
$$;