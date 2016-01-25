CREATE USER whatsopen;
CREATE SCHEMA whatsopen;
GRANT USAGE ON SCHEMA whatsopen TO whatsopen;
GRANT SELECT ON ALL TABLES IN SCHEMA whatsopen TO whatsopen;
GRANT EXECUTE ON ALL FUNCTIONS IN SCHEMA whatsopen TO whatsopen;

CREATE TABLE whatsopen.stores (
    id SERIAL PRIMARY KEY NOT NULL,
    name text NOT NULL,
    location text
);

CREATE TABLE whatsopen.hours (
    store_id integer REFERENCES stores NOT NULL,
    day timestamp without time zone NOT NULL,
    open time without time zone,
    close time without time zone
);

CREATE TABLE whatsopen.gen_hours (
    store_id integer REFERENCES stores NOT NULL,
    day double precision NOT NULL,
    open time without time zone,
    close time without time zone
);

CREATE FUNCTION whatsopen.get_timestamp(seconds double precision) RETURNS timestamp without time zone AS $$
    SELECT DATE 'epoch' + seconds * INTERVAL '1 second';
$$ LANGUAGE SQL;

CREATE FUNCTION whatsopen.get_seconds(stamp timestamp without time zone) RETURNS double precision AS $$
    SELECT EXTRACT(EPOCH FROM stamp);
$$ LANGUAGE SQL;

CREATE TYPE openclose AS (
    open time without time zone,
    close time without time zone
);

-- takes a time and store_id
-- returns (open timestamp, close timestamp) for that store on that day
-- takes into account stored exception days 
CREATE FUNCTION whatsopen.get_hours(t timestamp without time zone, s integer) RETURNS SETOF openclose AS $$
    BEGIN
        RETURN QUERY SELECT hours.open, hours.close FROM whatsopen.hours WHERE day = date_trunc('day', t) AND store_id = s; -- Return the hours for that day
        IF NOT FOUND THEN
            RETURN QUERY SELECT gen_hours.open, gen_hours.close FROM whatsopen.gen_hours WHERE day = EXTRACT(DOW FROM t) AND store_id = s; -- Return the general hours for that store
        END IF;
    END;
$$ LANGUAGE PLPGSQL;
