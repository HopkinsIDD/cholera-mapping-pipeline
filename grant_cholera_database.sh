echo "What is the name of the user?"
# read USERNAME
USERNAME=$1
run_psql(){
  echo $1
  sudo -u postgres psql cholera_covariates -c "$1"
}
run_psql "CREATE USER $USERNAME WITH PASSWORD 'dockertest' LOGIN;"
run_psql "GRANT CONNECT ON DATABASE cholera_covariates TO $USERNAME;"

run_psql "DO \$do$ \
DECLARE \
    sch text; \
BEGIN \
    FOR sch IN SELECT nspname FROM pg_namespace where nspname != 'pg_toast'  \
    and nspname != 'pg_temp_1' and nspname != 'pg_toast_temp_1' \
    and nspname != 'pg_statistic' and nspname != 'pg_catalog' \
    and nspname != 'information_schema' \
    LOOP \
        EXECUTE format(\$\$ GRANT USAGE ON SCHEMA %I TO $USERNAME \$\$, sch); \
        EXECUTE format(\$\$ GRANT SELECT ON ALL SEQUENCES IN SCHEMA %I TO $USERNAME \$\$, sch); \
        EXECUTE format(\$\$ GRANT SELECT ON ALL TABLES IN SCHEMA %I TO $USERNAME \$\$, sch); \
        EXECUTE format(\$\$ ALTER DEFAULT PRIVILEGES IN SCHEMA %I GRANT SELECT ON TABLES TO $USERNAME \$\$, sch); \
        EXECUTE format(\$\$ ALTER DEFAULT PRIVILEGES IN SCHEMA %I GRANT SELECT ON SEQUENCES TO $USERNAME \$\$, sch); \
    END LOOP; \
END; \
\$do$;"
run_psql "GRANT ALL PRIVILEGES ON SCHEMA public TO $USERNAME;"
run_psql "GRANT ALL PRIVILEGES ON SCHEMA grids TO $USERNAME;"
run_psql "GRANT ALL PRIVILEGES ON SCHEMA data TO $USERNAME;"
run_psql "GRANT ALL PRIVILEGES ON SCHEMA covariates TO $USERNAME;"
run_psql "GRANT ALL PRIVILEGES ON TABLESPACE fastspace TO $USERNAME;"
run_psql "ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT ALL PRIVILEGES ON TABLES TO $USERNAME;"
run_psql "ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT ALL PRIVILEGES ON SEQUENCES TO $USERNAME;"
