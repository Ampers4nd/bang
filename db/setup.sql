--------------
--
-- Eric V Schow
--
-- screw postgis for now, i'll just use lat, lon to start...
-- requires postgis package
-- to use postgis, must enable topology like this:
-- psql -f /path/to/topology.sql <db_name>
--
--------------

CREATE TABLE users (
    user_id SERIAL PRIMARY KEY,
    uname varchar(64) UNIQUE,
    hash char(256)
);
 
CREATE TABLE rooms (
    room_id SERIAL PRIMARY KEY,
    room_name varchar(64),
    is_open boolean,
    max_size integer,
    latitude float,
    longitude float
);

CREATE TABLE messages (
    message_id SERIAL PRIMARY KEY,
    created timestamp,
    expires timestamp,
    room_id integer REFERENCES rooms,
    from_id integer REFERENCES users,
    for_id integer REFERENCES users,
    is_active boolean,
    message varchar(140),
    media_url text
);

CREATE TABLE room_user (
    room_id integer REFERENCES rooms,
    user_id integer REFERENCES users,
    entered timestamp,
    exited timestamp,
    PRIMARY KEY (room_id, user_id, entered)
);
CREATE INDEX room_user_users ON room_user USING hash(user_id);
CREATE INDEX room_user_rooms ON room_user USING hash(room_id);
CREATE INDEX room_user_entered ON room_user USING btree(entered);
CREATE INDEX room_user_exited ON room_user USING btree(exited);

CREATE TABLE user_location (
    user_id integer REFERENCES users,
    latitude float,
    longitude float,
    update_time timestamp,
    PRIMARY KEY (user_id, update_time)
);
CREATE INDEX user_location_lat ON user_location USING btree(latitude);
CREATE INDEX user_location_lon ON user_location USING btree(longitude);
CREATE INDEX user_location_when ON user_location USING btree(when);
