library(duckdb)
library(bcrypt)
library(duckdb)
library(stringr)
library(leaflet)
library(dplyr)
library(dbplyr)
library(tidyr)
library(purrr)

source("upload_data_ui.R")
source("upload_data.R")

options(shiny.maxRequestSize=100*1024^2)

if(file.exists("./users.db")){
  userdb <- dbConnect(duckdb(), "./users.db")
}else{
  userdb <- dbConnect(duckdb(), "./users.db")
  dbExecute(userdb, "CREATE TABLE users (
    id INTEGER PRIMARY KEY,
    username VARCHAR(50) NOT NULL UNIQUE,
    email VARCHAR(500),
    password_hash VARCHAR(255) NOT NULL
  );")
  dbExecute(userdb, "CREATE SEQUENCE seq_personid START 1;")
  password <- "mysecretpassword"
  hashed_password <-

  dbExecute(userdb, "INSERT INTO users (id, username, password_hash) VALUES (nextval('seq_personid'), ?, ?)", list("admin", hashpw(password, gensalt())))
}

if(file.exists("./file_uploads.db")){
  file_uploads <- dbConnect(duckdb(), "./file_uploads.db")
}else{
  file_uploads <- dbConnect(duckdb(), "./file_uploads.db")
  dbExecute(file_uploads, "CREATE TABLE file_uploads (
    id INTEGER PRIMARY KEY,
    username VARCHAR(50),
    date DATE,
    speedLimit INTEGER,
    notes TEXT,
    location DOUBLE[2],
    files VARCHAR[]
  );")
  dbExecute(file_uploads, "CREATE SEQUENCE seq_file_upload_id START 1;")
}

