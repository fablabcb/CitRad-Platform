\l

CREATE DATABASE users;
CREATE DATABASE content;

CREATE SEQUENCE seq_file_upload_id
START 1
INCREMENT 1
MINVALUE 1
MAXVALUE 2147483647
CACHE 1;

CREATE TABLE "sensor_locations" (
    "id" SERIAL NOT NULL,
    PRIMARY KEY ("id"),
    "username" character(200) NOT NULL,
    "date_created" timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "user_speedlimit" integer,
    "direction" integer,
    "notes" text,
    "street_name" text,
    "street_name:hsb" text,
    "osm_speedlimit" integer,
    "oneway" boolean,
    "lanes" integer,
    "location_geom" geometry(Point,4326),
    "street_geom" geometry(LineString,4326)
);


CREATE TABLE "file_uploads" (
    "id" SERIAL NOT NULL,
    PRIMARY KEY ("id"),
    "username" character(200) NOT NULL,
    "upload_date" timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "notes" text,
    "temporary_speedlimit" integer,
    "location_id" integer NOT NULL,
    "filename" text,
    CONSTRAINT "fk_location_id" FOREIGN KEY ("location_id") REFERENCES "sensor_locations" ("id")
);

CREATE TABLE users (
    id INTEGER PRIMARY KEY,
    username VARCHAR(50) NOT NULL UNIQUE,
    email VARCHAR(500),
    password_hash VARCHAR(255) NOT NULL
);


GRANT ALL PRIVILEGES ON DATABASE users TO data_platform;
GRANT ALL PRIVILEGES ON DATABASE content TO data_platform;
GRANT ALL PRIVILEGES ON TABLE sensor_locations TO data_platform;
GRANT ALL PRIVILEGES ON SCHEMA public TO data_platform;
GRANT ALL PRIVILEGES ON SEQUENCE sensor_locations_id_seq TO data_platform;
GRANT ALL PRIVILEGES ON TABLE file_uploads TO data_platform;
GRANT ALL PRIVILEGES ON SEQUENCE file_uploads_id_seq TO data_platform;
