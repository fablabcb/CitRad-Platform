psql -U postgres -h db -p 5432


\l
\c content

CREATE DATABASE users;
CREATE DATABASE content;
CREATE ROLE data_platform WITH LOGIN PASSWORD '';
GRANT ALL PRIVILEGES ON DATABASE users TO data_platform;
GRANT ALL PRIVILEGES ON DATABASE content TO data_platform;
GRANT ALL PRIVILEGES ON SCHEMA public TO data_platform;


DROP table sensor_locations;
DROP table file_uploads CASCADE;
DROP table bin_index;
DROP table raw_metrics;
DROP table car_detections;

TRUNCATE table car_detections;
TRUNCATE table raw_metrics;
TRUNCATE table bin_index;
TRUNCATE table file_uploads CASCADE;



\c users
DROP table users;
CREATE TABLE users (
    "id" SERIAL PRIMARY KEY NOT NULL,
    "username" VARCHAR(50) NOT NULL UNIQUE,
    "email" VARCHAR(500),
    "password_hash" VARCHAR(255) NOT NULL,
    "email_confirmed" BOOLEAN DEFAULT FALSE,
    "activated" BOOLEAN DEFAULT FALSE,
    "user_admin" BOOLEAN DEFAULT FALSE,
    "location_admin" BOOLEAN DEFAULT FALSE,
    "data_admin" BOOLEAN DEFAULT FALSE,
    "validation"  BOOLEAN DEFAULT FALSE
);
GRANT ALL PRIVILEGES ON TABLE users TO data_platform;
GRANT ALL PRIVILEGES ON SEQUENCE users_id_seq TO data_platform;

DROP table email_confirmations;
CREATE TABLE email_confirmations (
  "code" VARCHAR(255) PRIMARY KEY NOT NULL,
  "user_id" integer NOT NULL,
  CONSTRAINT "fk_user_id" FOREIGN KEY ("user_id") REFERENCES "users" ("id")
);
GRANT ALL PRIVILEGES ON TABLE email_confirmations TO data_platform;


\c content
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
GRANT ALL PRIVILEGES ON TABLE sensor_locations TO data_platform;
GRANT ALL PRIVILEGES ON SEQUENCE sensor_locations_id_seq TO data_platform;


CREATE TYPE filetype AS ENUM ('spectrum', 'metrics', 'car_detections', 'image');
CREATE TABLE "file_uploads" (
    "id" SERIAL NOT NULL,
    PRIMARY KEY ("id"),
    "username" character(200) NOT NULL,
    "upload_date" timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "notes" text,
    "temporary_speedlimit" integer,
    "location_id" integer NOT NULL,
    "filename" text,
    "filetype" filetype,
    "file_version" integer,
    "iq_measurement" boolean,
    "sample_rate" integer,
    "num_fft_bins" integer,
    "start_time" timestamp,
    "end_time" timestamp,
    "indexed" boolean DEFAULT false,
    "processed" boolean DEFAULT false,
    "hash" character(200)
    CONSTRAINT "fk_location_id" FOREIGN KEY ("location_id") REFERENCES "sensor_locations" ("id")
);
GRANT ALL PRIVILEGES ON TABLE file_uploads TO data_platform;
GRANT ALL PRIVILEGES ON SEQUENCE file_uploads_id_seq TO data_platform;


CREATE TABLE "bin_index" (
    "location_id" integer NOT NULL,
    "file_id" integer NOT NULL,
    "timestamp" timestamp NOT NULL,
    "milliseconds" integer,
    "byte_index" integer,
    CONSTRAINT "fk_location_id" FOREIGN KEY ("location_id") REFERENCES "sensor_locations" ("id"),
    CONSTRAINT "pk_bin_data" PRIMARY KEY ("file_id", "timestamp"),
    CONSTRAINT "fk_file_id" FOREIGN KEY ("file_id") REFERENCES "file_uploads" ("id")
);
GRANT ALL PRIVILEGES ON TABLE bin_index TO data_platform;



CREATE TABLE "raw_metrics" (
    "file_id" integer NOT NULL,
    "location_id" integer NOT NULL,
    "timestamp" timestamp NOT NULL,
    "milliseconds" integer,
    "speed" real,
    "speed_reverse" real,
    "strength" real,
    "strength_reverse" real,
    "meanAmplitudeForPedestrians" real,
    "meanAmplitudeForCars" real,
    "meanAmplitudeForNoiseLevel" real,
    "dynamic_noise_level" real,
    "dynamic_noise_level_filter_size" integer,
    "car_trigger_signal" real,
    "car_trigger_signal_filter_size" integer,
    CONSTRAINT "fk_location_id" FOREIGN KEY ("location_id") REFERENCES "sensor_locations" ("id"),
    CONSTRAINT "pk_raw_metrics" PRIMARY KEY ("file_id", "timestamp"),
    CONSTRAINT "fk_file_id" FOREIGN KEY ("file_id") REFERENCES "file_uploads" ("id")
);
GRANT ALL PRIVILEGES ON TABLE raw_metrics TO data_platform;



CREATE TYPE car_detection_source AS ENUM ('sensor unit', 'R script');
CREATE TABLE "car_detections" (
  "id" SERIAL NOT NULL,
  "location_id" integer NOT NULL,
  "timestamp" timestamp NOT NULL,
  "milliseconds" integer,
  "isForward" integer,
  "sampleCount" integer,
  "medianSpeed" real,
  "source" car_detection_source DEFAULT 'sensor unit',
  "file_id" integer NOT NULL,
  CONSTRAINT "fk_location_id" FOREIGN KEY ("location_id") REFERENCES "sensor_locations" ("id"),
  CONSTRAINT "pk_car_detections" PRIMARY KEY ("file_id", "timestamp"),
  CONSTRAINT "fk_file_id" FOREIGN KEY ("file_id") REFERENCES "file_uploads" ("id")
);
GRANT ALL PRIVILEGES ON TABLE car_detections TO data_platform;
GRANT ALL PRIVILEGES ON SEQUENCE car_detections_id_seq TO data_platform;
