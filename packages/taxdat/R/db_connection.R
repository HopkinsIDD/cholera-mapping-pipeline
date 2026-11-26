# Connection layer for the cholera_covariates PostGIS database.
#
# Every consumer (DBI, psql, raster2pgsql | psql, GDAL's PostGISRaster driver)
# derives its settings from the same libpq environment variables, so a job only
# has to export PGHOST / PGPORT / PGDATABASE / PGUSER / PGPASSWORD (or use a
# ~/.pgpass file). Passwords never appear in a command line or a log message.

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || identical(a, "")) b else a

#' @title Get database configuration
#' @name get_db_config
#' @description Resolves the covariates database settings from libpq environment
#' variables. `PGHOST` is required; there is no silent fallback to localhost.
#'
#' @param dbname optional database name overriding `PGDATABASE`
#' @param dbuser optional user name overriding `PGUSER`
#'
#' @return a list of class `cholera_db_config` with host, port, dbname, user,
#' password (NULL when libpq should resolve it), work_schema and search_path
#' @export
get_db_config <- function(dbname = NULL, dbuser = NULL) {
  host <- Sys.getenv("PGHOST", "")
  if (host == "") {
    stop("PGHOST is not set. Export PGHOST (and PGPORT, PGDATABASE, PGUSER) ",
         "before connecting to the covariates database; use PGHOST=localhost ",
         "for a local server.")
  }

  password <- Sys.getenv("PGPASSWORD", "")
  if (password == "") {
    legacy <- Sys.getenv("COVARIATE_DATABASE_PASSWORD", "")
    if (legacy != "") {
      warning("COVARIATE_DATABASE_PASSWORD is deprecated; set PGPASSWORD or use ~/.pgpass.",
              call. = FALSE)
      password <- legacy
    }
  }

  config <- list(
    host = host,
    port = as.integer(Sys.getenv("PGPORT", "5432")),
    dbname = dbname %||% Sys.getenv("PGDATABASE", "cholera_covariates"),
    user = dbuser %||% Sys.getenv("PGUSER", Sys.getenv("USER", "")),
    password = if (password == "") NULL else password,
    work_schema = Sys.getenv("CHOLERA_DB_WORK_SCHEMA", "runs"),
    search_path = c(Sys.getenv("CHOLERA_DB_WORK_SCHEMA", "runs"),
                    "public", "grids", "covariates", "data")
  )
  class(config) <- "cholera_db_config"
  config
}

#' @export
format.cholera_db_config <- function(x, ...) {
  sprintf("postgresql://%s@%s:%s/%s (password %s)",
          x$user, x$host, x$port, x$dbname,
          if (is.null(x$password)) "from libpq" else "set")
}

#' @export
print.cholera_db_config <- function(x, ...) {
  cat(format(x), "\n")
  invisible(x)
}

#' @title Export libpq environment variables
#' @name export_pg_env
#' @description Makes child processes (psql, raster2pgsql, gdalwarp) see the same
#' connection settings as R, without passing them on the command line.
#'
#' @param cfg a `cholera_db_config`
#' @return the config, invisibly
#' @export
export_pg_env <- function(cfg = get_db_config()) {
  Sys.setenv(PGHOST = cfg$host,
             PGPORT = as.character(cfg$port),
             PGDATABASE = cfg$dbname,
             PGUSER = cfg$user,
             PGOPTIONS = paste0("-c search_path=", paste(cfg$search_path, collapse = ",")))
  if (!is.null(cfg$password)) {
    Sys.setenv(PGPASSWORD = cfg$password)
  }
  invisible(cfg)
}

#' @title Connect to database
#' @name connect_to_db
#' @description Opens a connection to the covariates database and sets the
#' search path so unqualified per-run tables land in the work schema.
#'
#' @param dbuser optional user name overriding `PGUSER`
#' @param dbname optional database name overriding `PGDATABASE`
#'
#' @return a DBI database connection object
#' @export
connect_to_db <- function(dbuser = NULL, dbname = NULL) {
  cfg <- get_db_config(dbname = dbname, dbuser = dbuser)
  args <- list(RPostgres::Postgres(),
               host = cfg$host, port = cfg$port,
               dbname = cfg$dbname, user = cfg$user)
  if (!is.null(cfg$password)) {
    args$password <- cfg$password
  }
  conn <- do.call(DBI::dbConnect, args)
  DBI::dbExecute(conn, paste("SET search_path TO",
                             paste(DBI::dbQuoteIdentifier(conn, cfg$search_path),
                                   collapse = ", ")))
  conn
}

#' @title Get database connection string
#' @name get_covariate_conn_string
#' @description Password-free connection URI, safe to print in logs.
#'
#' @param dbuser optional user name overriding `PGUSER`
#' @return a character string
#' @export
get_covariate_conn_string <- function(dbuser = NULL) {
  cfg <- get_db_config(dbuser = dbuser)
  glue::glue("postgresql://{cfg$user}@{cfg$host}:{cfg$port}/{cfg$dbname}")
}

#' @title GDAL PostGIS raster data source name
#' @name get_pg_gdal_dsn
#' @description Builds a `PG:` string for GDAL's PostGISRaster driver. The
#' password is read by libpq from the environment, never embedded.
#'
#' @param schema schema of the raster table
#' @param table raster table name
#' @param mode GDAL PostGISRaster mode (2 = one raster per table)
#' @return a character string (unquoted; quote with shQuote when used in a shell)
#' @export
get_pg_gdal_dsn <- function(schema, table, mode = 2) {
  cfg <- get_db_config()
  glue::glue("PG:host={cfg$host} port={cfg$port} dbname={cfg$dbname} ",
             "user={cfg$user} schema={schema} table={table} mode={mode}")
}

#' @title Execute a statement and release the result
#' @name db_exec
#' @param conn DBI connection
#' @param statement SQL string (already interpolated with glue_sql)
#' @return number of affected rows, invisibly
#' @export
db_exec <- function(conn, statement) {
  invisible(DBI::dbExecute(conn, statement))
}

#' @title Run identifier
#' @name get_run_id
#' @description Identifier used to suffix scratch tables so concurrent jobs do
#' not collide.
#' @return character
#' @export
get_run_id <- function() {
  id <- Sys.getenv("CHOLERA_RUN_ID", Sys.getenv("SLURM_JOB_ID", ""))
  if (id == "") {
    id <- as.character(Sys.getpid())
  }
  gsub("[^A-Za-z0-9_]", "_", id)
}

#' @title Scratch table name
#' @name scratch_table_name
#' @param prefix short prefix, e.g. "tmprast"
#' @return schema-qualified name in the work schema
#' @export
scratch_table_name <- function(prefix) {
  paste0(get_db_config()$work_schema, ".", prefix, "_", get_run_id())
}

# Command wrappers ------------------------------------------------------------

#' @title Container-aware command
#' @name pg_tool_cmd
#' @description Returns the argv to run a PostGIS command-line tool. When
#' `CHOLERA_SIF` points to an Apptainer image, the tool runs from that image
#' (the image passes the host environment, so libpq variables are visible).
#'
#' @param tool name of the binary, e.g. "psql" or "raster2pgsql"
#' @param args character vector of arguments
#' @return a character vector, first element the executable
#' @export
pg_tool_cmd <- function(tool, args = character()) {
  sif <- Sys.getenv("CHOLERA_SIF", "")
  if (sif != "") {
    binds <- Sys.getenv("CHOLERA_SIF_BINDS", "")
    c("apptainer", "exec", if (binds != "") c("--bind", binds), sif, tool, args)
  } else {
    c(tool, args)
  }
}

#' @title psql command
#' @name psql_cmd
#' @param extra extra arguments appended after the connection arguments
#' @return argv vector
#' @export
psql_cmd <- function(extra = character()) {
  cfg <- get_db_config()
  pg_tool_cmd("psql", c("-v", "ON_ERROR_STOP=1", "-q",
                        "-h", cfg$host, "-p", cfg$port,
                        "-U", cfg$user, "-d", cfg$dbname, extra))
}

#' @title Run a command and fail loudly
#' @name run_cmd
#' @description Runs an argv vector through the shell with every element quoted.
#' Stops with the captured output if the exit status is non-zero. The command
#' line never contains a password (see `get_db_config`).
#'
#' @param argv character vector, first element the executable
#' @param label short label for messages
#' @return captured output lines, invisibly
#' @export
run_cmd <- function(argv, label = argv[1]) {
  out <- suppressWarnings(system2(argv[1], shQuote(argv[-1]),
                                  stdout = TRUE, stderr = TRUE))
  status <- attr(out, "status") %||% 0L
  if (status != 0) {
    stop(label, " failed with status ", status, ":\n",
         paste(utils::tail(out, 30), collapse = "\n"), call. = FALSE)
  }
  invisible(out)
}

#' @title Load a raster file into PostGIS with raster2pgsql
#' @name raster2pgsql_pipe
#' @description Runs `raster2pgsql ... | psql` with `pipefail`, so a failure on
#' either side stops R. Connection settings come from libpq variables.
#'
#' @param file raster file (GeoTIFF or NetCDF)
#' @param table schema-qualified target table
#' @param mode "create" (drop and recreate), "append", or "prepare"
#' @param srid SRID to assign
#' @param tile tile size passed to `-t`
#' @param index create a GiST index (`-I`)
#' @param constraints add raster constraints (`-C`)
#' @return NULL, invisibly
#' @export
raster2pgsql_pipe <- function(file, table, mode = c("create", "append", "prepare"),
                              srid = 4326, tile = "auto", index = TRUE,
                              constraints = FALSE) {
  mode <- match.arg(mode)
  if (!file.exists(file)) {
    stop("raster2pgsql_pipe: file not found: ", file)
  }
  export_pg_env()
  mode_flag <- c(create = "-d", append = "-a", prepare = "-p")[[mode]]
  r2p <- pg_tool_cmd("raster2pgsql",
                     c("-s", srid, if (index) "-I", if (constraints) "-C",
                       "-t", tile, mode_flag, file, table))
  psql <- psql_cmd()
  pipeline <- paste("set -o pipefail;",
                    paste(shQuote(r2p), collapse = " "), "|",
                    paste(shQuote(psql), collapse = " "), "> /dev/null")
  message("-- raster2pgsql ", basename(file), " -> ", table, " (", mode, ")")
  run_cmd(c("bash", "-c", pipeline), label = paste("raster2pgsql", basename(file)))
}

# Raster loading ---------------------------------------------------------------

#' @title Which raster loader to use
#' @name raster_loader
#' @description `CHOLERA_RASTER_LOADER`: "raster2pgsql", "dbi", or "auto"
#' (default: raster2pgsql when it is on the PATH and no container image is set,
#' otherwise dbi). The postgis/postgis image used on the cluster does not ship
#' raster2pgsql, so jobs there use the dbi loader.
#' @return "raster2pgsql" or "dbi"
#' @export
raster_loader <- function() {
  m <- Sys.getenv("CHOLERA_RASTER_LOADER", "auto")
  if (m == "auto") {
    m <- if (Sys.getenv("CHOLERA_SIF") == "" && nzchar(Sys.which("raster2pgsql"))) "raster2pgsql" else "dbi"
  }
  match.arg(m, c("raster2pgsql", "dbi"))
}

#' @title Load a raster file into PostGIS
#' @name load_raster_to_db
#' @description Loads a raster (GeoTIFF, NetCDF or VRT, any number of bands)
#' into a table `(rid serial, rast raster)` tiled `tile` x `tile`, with a GiST
#' index on the tiles' convex hulls and optionally the raster constraints.
#'
#' The "dbi" loader needs no PostGIS client tools: it cuts the raster into
#' chunks with gdal_translate and sends each chunk to the server, which decodes
#' it with ST_FromGDALRaster and splits it with ST_Tile (the database must
#' allow the GTiff driver, see hpc/yggdrasil/sql/02_schemas_grants.sql).
#'
#' @param file raster file
#' @param table schema-qualified target table
#' @param mode "create" (drop and recreate) or "append"
#' @param srid SRID to assign
#' @param tile tile size in pixels
#' @param index create the GiST index
#' @param constraints add raster constraints
#' @param conn optional DBI connection (dbi loader)
#' @param chunk chunk size in pixels (multiple of `tile`)
#' @param skip_empty drop tiles where every band is NoData (ocean, outside the
#'   area of interest's mask). Extraction is by geometry, so missing tiles read
#'   as NoData; a global 1 km population table shrinks to about a third.
#' @return NULL, invisibly
#' @export
load_raster_to_db <- function(file, table, mode = c("create", "append"), srid = 4326,
                              tile = 128, index = TRUE, constraints = FALSE,
                              conn = NULL, chunk = 1024, skip_empty = TRUE) {
  mode <- match.arg(mode)
  if (raster_loader() == "raster2pgsql") {
    return(raster2pgsql_pipe(file, table, mode = mode, srid = srid,
                             index = index, constraints = constraints))
  }
  if (!file.exists(file)) stop("load_raster_to_db: file not found: ", file)
  if (is.null(conn)) {
    conn <- connect_to_db()
    on.exit(DBI::dbDisconnect(conn), add = TRUE)
  }
  tbl <- sql_table(conn, table)
  idx <- DBI::dbQuoteIdentifier(conn, paste0(table_part(table), "_st_convexhull_idx"))
  spec <- gdal_grid_spec(file)
  message("-- loading ", basename(file), " -> ", table, " (", mode, ", ",
          spec$ncol, " x ", spec$nrow, " px)")

  if (mode == "create") {
    db_exec(conn, glue::glue_sql("DROP TABLE IF EXISTS {tbl};", .con = conn))
    db_exec(conn, glue::glue_sql("CREATE TABLE {tbl} (rid serial PRIMARY KEY, rast raster);", .con = conn))
  }
  part <- tempfile(fileext = ".tif")
  on.exit(unlink(part), add = TRUE)
  for (y in seq(0, spec$nrow - 1, by = chunk)) {
    for (x in seq(0, spec$ncol - 1, by = chunk)) {
      w <- min(chunk, spec$ncol - x)
      h <- min(chunk, spec$nrow - y)
      run_cmd(c("gdal_translate", "-q", "-of", "GTiff", "-co", "COMPRESS=DEFLATE",
                "-srcwin", x, y, w, h, file, part), label = "gdal_translate")
      bytes <- readBin(part, "raw", file.size(part))
      DBI::dbExecute(conn, glue::glue_sql(
        "INSERT INTO {tbl} (rast)
         SELECT t FROM (SELECT ST_Tile(ST_FromGDALRaster($1, $2), $3, $3) AS t) s
         WHERE NOT $4 OR EXISTS (SELECT 1 FROM generate_series(1, ST_NumBands(t)) b
                                 WHERE NOT ST_BandIsNoData(t, b, true));", .con = conn),
        params = list(blob::blob(bytes), as.integer(srid), as.integer(tile), isTRUE(skip_empty)))
    }
  }
  if (index && mode == "create") {
    db_exec(conn, glue::glue_sql("CREATE INDEX {idx} ON {tbl} USING gist (ST_ConvexHull(rast));", .con = conn))
  }
  if (constraints) {
    parts <- strsplit(table, ".", fixed = TRUE)[[1]]
    db_exec(conn, glue::glue_sql("SELECT AddRasterConstraints({parts[1]}::name, {parts[2]}::name, 'rast'::name);",
                                 .con = conn))
  }
  db_exec(conn, glue::glue_sql("ANALYZE {tbl};", .con = conn))
  invisible(NULL)
}
