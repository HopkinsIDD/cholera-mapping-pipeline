#!/bin/bash
set -e  # if fail the script will stop

# Initialize PostgreSQL in container
su postgres -c "/usr/lib/postgresql/17/bin/initdb -D /var/lib/postgresql/data"

# start PostgreSQL
su postgres -c "/usr/lib/postgresql/17/bin/pg_ctl -D /var/lib/postgresql/data -l /var/lib/postgresql/logfile start"

# wait PostgreSQL
sleep 5

# create database and user
su - postgres -c "psql -c \"CREATE DATABASE cholera_covariates;\""
su - postgres -c "psql -c \"CREATE USER app WITH LOGIN;\""
su - postgres -c "psql -c \"GRANT ALL ON DATABASE cholera_covariates TO app;\""

# Install PostGIS extension 
su - postgres -c "psql -d cholera_covariates -c \"CREATE EXTENSION postgis;\""
su - postgres -c "psql -d cholera_covariates -c \"CREATE EXTENSION postgis_raster;\""
su - postgres -c "psql -d cholera_covariates -c \"CREATE EXTENSION postgis_topology;\""
su - postgres -c "psql -d cholera_covariates -c \"CREATE EXTENSION postgis_sfcgal;\""
su - postgres -c "psql -d cholera_covariates -c \"CREATE EXTENSION fuzzystrmatch;\""
su - postgres -c "psql -d cholera_covariates -c \"CREATE EXTENSION address_standardizer;\""
su - postgres -c "psql -d cholera_covariates -c \"CREATE EXTENSION address_standardizer_data_us;\""
su - postgres -c "psql -d cholera_covariates -c \"CREATE EXTENSION postgis_tiger_geocoder;\""

# create schema
su - app -c "psql -d cholera_covariates -c \"CREATE SCHEMA covariates;\""
su - app -c "psql -d cholera_covariates -c \"CREATE SCHEMA data;\""
su - app -c "psql -d cholera_covariates -c \"CREATE SCHEMA grids;\""
