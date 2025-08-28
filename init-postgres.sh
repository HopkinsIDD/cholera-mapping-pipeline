#!/bin/bash
set -e  # 如果任何命令失败，则退出脚本

# 初始化 PostgreSQL 数据库
su postgres -c "/usr/lib/postgresql/17/bin/initdb -D /var/lib/postgresql/data"

# 启动 PostgreSQL
su postgres -c "/usr/lib/postgresql/17/bin/pg_ctl -D /var/lib/postgresql/data -l /var/lib/postgresql/logfile start"

# 等待 PostgreSQL 启动
sleep 5

# 创建数据库和用户
su - postgres -c "psql -c \"CREATE DATABASE cholera_covariates;\""
su - postgres -c "psql -c \"CREATE USER app WITH LOGIN;\""
su - postgres -c "psql -c \"GRANT ALL ON DATABASE cholera_covariates TO app;\""

# 安装 PostGIS 扩展
su - postgres -c "psql -d cholera_covariates -c \"CREATE EXTENSION postgis;\""
su - postgres -c "psql -d cholera_covariates -c \"CREATE EXTENSION postgis_raster;\""
su - postgres -c "psql -d cholera_covariates -c \"CREATE EXTENSION postgis_topology;\""
su - postgres -c "psql -d cholera_covariates -c \"CREATE EXTENSION postgis_sfcgal;\""
su - postgres -c "psql -d cholera_covariates -c \"CREATE EXTENSION fuzzystrmatch;\""
su - postgres -c "psql -d cholera_covariates -c \"CREATE EXTENSION address_standardizer;\""
su - postgres -c "psql -d cholera_covariates -c \"CREATE EXTENSION address_standardizer_data_us;\""
su - postgres -c "psql -d cholera_covariates -c \"CREATE EXTENSION postgis_tiger_geocoder;\""

# 创建应用 schema
su - app -c "psql -d cholera_covariates -c \"CREATE SCHEMA covariates;\""
su - app -c "psql -d cholera_covariates -c \"CREATE SCHEMA data;\""
su - app -c "psql -d cholera_covariates -c \"CREATE SCHEMA grids;\""
