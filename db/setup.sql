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

CREATE TYPE user_type AS ENUM ('bang', 'fb', 'gp', 'yahoo');

CREATE TABLE users (
    user_id SERIAL PRIMARY KEY,
    uname varchar(64) UNIQUE,
    user_type user_type DEFAULT 'bang',
    pw_hash char(128)
);
CREATE INDEX users_user_type ON users USING btree(user_type);

CREATE TYPE gender AS ENUM ('m', 'f', 'na');
CREATE TABLE user_info (
    email varchar(128) UNIQUE,
    nickname varchar(32) UNIQUE,
    dob timestamp,
    gender gender DEFAULT 'na',
    user_id REFERENCES users
);
CREATE INDEX user_info_dob ON user_info USING btree(dob);
CREATE INDEX user_info_gender ON user_info USING btree(gender);

CREATE TABLE user_stats (
    user_id REFERENCES users,
    token char(32),
    device_id varchar(32),
    ip_address inet,
    issued timestamp,
    expires timestamp
);
CREATE INDEX user_stats_token ON user_stats USING hash(token);
CREATE INDEX user_stats_expires ON user_stats USING btree(expires);

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
