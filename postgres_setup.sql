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
DROP table file_uploads;
DROP table bin_index;
DROP table raw_metrics;
DROP table car_detections;

CREATE TABLE users (
    id INTEGER PRIMARY KEY,
    username VARCHAR(50) NOT NULL UNIQUE,
    email VARCHAR(500),
    password_hash VARCHAR(255) NOT NULL
);

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


CREATE TABLE "file_uploads" (
    "id" SERIAL NOT NULL,
    PRIMARY KEY ("id"),
    "username" character(200) NOT NULL,
    "upload_date" timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "notes" text,
    "temporary_speedlimit" integer,
    "location_id" integer NOT NULL,
    "filename" text,
    "filetype" character(5),
    "file_version" integer,
    "iq_measurement" boolean,
    "sample_rate" integer,
    "num_fft_bins" integer,
    "start_time" timestamp,
    "end_time" timestamp,
    "indexed" boolean DEFAULT false,
    "processed" boolean DEFAULT false,
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
