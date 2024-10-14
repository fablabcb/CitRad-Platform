\l

CREATE DATABASE users;
CREATE DATABASE content;

CREATE SEQUENCE seq_file_upload_id
START 1
INCREMENT 1
MINVALUE 1
MAXVALUE 2147483647
CACHE 1;

CREATE TABLE "file_uploads" (
    "id" SERIAL NOT NULL,
    PRIMARY KEY ("id"),
    "username" character(200) NOT NULL,
    "date" timestamp NOT NULL,
    "speedlimit" integer,
    "direction" integer,
    "notes" text,
    "location" geometry(Point,4326),
    "name" text,
    "name:hsb" text,
    "osm_speedlimit" integer,
    "oneway" boolean,
    "lanes" integer,
    "street_geom" geometry(LineString,4326),
    "files" text[]
);

CREATE TABLE users (
  id INTEGER PRIMARY KEY,
  username VARCHAR(50) NOT NULL UNIQUE,
  email VARCHAR(500),
  password_hash VARCHAR(255) NOT NULL
);

CREATE ROLE data_platform WITH LOGIN PASSWORD '3z681LWEwlEcpl4Xp4zJ4Dx';
GRANT ALL PRIVILEGES ON DATABASE users TO data_platform;
GRANT ALL PRIVILEGES ON DATABASE content TO data_platform;
GRANT ALL PRIVILEGES ON TABLE file_uploads TO data_platform;
GRANT ALL PRIVILEGES ON SCHEMA public TO data_platform;
GRANT ALL PRIVILEGES ON SEQUENCE file_uploads_id_seq TO data_platform;
