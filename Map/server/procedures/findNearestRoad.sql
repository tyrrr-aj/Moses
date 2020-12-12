create or replace function findNearestRoad(
    lon IN double precision,
    lat IN double precision
)
    returns table(
        "id" bigint,
        "isRoad" bool,
        "partOfRoad" double precision
             )
    language plpgsql
as
    $$
    declare
        road record;
        junction_id bigint;
    begin
        select
            "Id",
            ST_Distance("Closest", "Beginning", false) / "Length" as "PartOfRoad",
            "Length",
            source,
            target
        into road
        from (
                 select
                        gid as "Id",
                        ST_ClosestPoint(the_geom, ST_SetSRID(ST_Point(lon, lat), 4326)) AS "Closest",
                                        ST_SetSRID(ST_Point(x1, y1), 4326) AS "Beginning",
                        "length_m" as "Length",
                        source,
                        target
                from ways
                where st_dwithin(the_geom, ST_SetSRID(ST_Point(lon, lat), 4326), 10.0)
                order by ST_Distance(the_geom, ST_SetSRID(ST_Point(lon, lat), 4326))
                limit 1
            ) closest_edge;

        junction_id = isOnJunction(road."Length", road."PartOfRoad", road.source, road.target);

        if junction_id = -1 then
            return query
                select road."Id", true, road."PartOfRoad";
        else
            return query
                select junction_id, false, 0.0::double precision;
        end if;
    end;
$$;