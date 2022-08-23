library(R6)

Randomizable <- R6Class(
  "Randomizable",
  private = list(
    .value = NULL,
    .randomize = function() {
      stop("This is an abstract method")
    }
  ),
  public = list(
    randomize = function() {
      private$.value <- private$.randomize()
      return(private$.value)
    }
  ),
  active = list(
    value = function(value) {
      if (missing(value)) {
        if (is.null(private$.value)) {
          self$randomize()
        }
        return(private$.value)
      }
      stop("Do not write directly to value")
    }
  )
)

DiscreteRandomizable <- R6Class(
  classname = "DiscreteRandomizable",
  inherit = Randomizable,
  private = list(
    .registry = list(),
    .randomize = function() {
      sample(private$.registry, 1)
    }
  ),
  public = list(
    initialize = function(options) {
      private$.registry <- options
    }
  )
)

MemberRandomizable <- R6Class(
  "MemberRandomizable",
  inherit = Randomizable,
  private = list(
    .registry = list(),
    .member_values = list(),
    .construct_from_members = function(member_list) {
      stop("This is an abstract method")
    },
    .randomize = function() {
      private$.member_values <- lapply(
        private$.registry,
        function(class) {
          class$new()$value
        }
      )
      return(private$.construct_from_members(private$.member_values))
    }
  ),
  public = list(
    initialize = function(member_classes) {
      private$.registry <- member_classes
    }
  )
)

String <- R6Class(
  classname = "PositiveInteger",
  inherit = Randomizable,
  private = list(
    .randomize = function() {
      nchar <- 5 + PositiveInteger$new()$value
      paste0(letters[round(runif(nchar, 0.5, 26.5))], collapse = "")
    }
  )
)

PositiveInteger <- R6Class(
  classname = "PositiveInteger",
  inherit = Randomizable,
  private = list(
    .randomize = function() {
      floor(abs(boot::logit(runif(1)))) + 1
    }
  )
)

NonNegativeInteger <- R6Class(
  classname = "NonNegativeInteger",
  inherit = Randomizable,
  private = list(
    .randomize = function() {
      floor(abs(boot::logit(runif(1))))
    }
  )
)

Date <- R6Class(
  classname = "Date",
  inherit = DiscreteRandomizable,
  public = list(
    initialize = function() {
      private$.registry <- lubridate::ymd(c("2000-01-01"))
    }
  )
)

Scale <- R6Class(
  classname = "Scale",
  inherit = DiscreteRandomizable,
  public = list(
    initialize = function() {
      private$.registry <- c("year")
    }
  )
)

Period <- R6Class(
  classname = "Period",
  inherit = MemberRandomizable,
  private = list(
    .construct_from_members = function(member_list) {
      lubridate::period(num = member_list[[1]], units = member_list[[2]])
    }
  ),
  public = list(
    initialize = function() {
      private$.registry <- list(PositiveInteger, Scale)
    }
  )
)

PolygonTemplate <- R6Class(
  classname = "PolygonTemplate",
  inherit = DiscreteRandomizable,
  public = list(
    initialize = function() {
      private$.registry <- c("full and grid")
    }
  )
)

Boolean <- R6Class(
  classname = "Boolean",
  inherit = DiscreteRandomizable,
  public = list(
    initialize = function() {
      private$.registry <- c(TRUE, FALSE)
    }
  )
)

Covariate <- R6Class(
  classname = "Covariate",
  inherit = MemberRandomizable,
  private = list(
    .construct_from_members = function(member_list) {
      return(list(
        template = "custom",
        name = member_list[["name"]],
        nonspatial = member_list[["nonspatial"]],
        nontemporal = member_list[["nontemporal"]],
        spatially_smooth = member_list[["spatially_smooth"]],
        temporally_smooth = member_list[["temporally_smooth"]],
        constant = member_list[["constant"]],
        radiating = member_list[["radiating"]],
        polygonal = member_list[["polygonal"]],
        include_in_simulation = member_list[["modeling"]],
        include_in_model = member_list[["simulating"]],
        seed = "global_seed"
      ))
    }
  ),
  public = list(
    initialize = function() {
      private$.registry <- list(
        name = String,
        nonspatial = Boolean,
        nontemporal = Boolean,
        spatially_smooth = Boolean,
        temporally_smooth = Boolean,
        constant = Boolean,
        radiating = Boolean,
        polygonal = Boolean,
        modeling = Boolean,
        simulating = Boolean
      )
    }
  )
)

CovariateList <- R6Class(
  classname = "CovariateList",
  inherit = Randomizable,
  private = list(
    .covariates = list(),
    .randomize = function() {
      ncovariates <- PositiveInteger$new()$value
      rc <- lapply(seq_len(ncovariates), function(x) {
        Covariate$new()$value
      })
      rc[[1]][["name"]] <- "population"
      rc[[1]][["include_in_simulation"]] <- TRUE
      rc[[1]][["include_in_model"]] <- TRUE
      return(rc)
    }
  )
)

ObservationTemplate <- R6Class(
  classname = "ObservationTemplate",
  inherit = DiscreteRandomizable,
  public = list(
    initialize = function() {
      private$.registry <- c("accurate")
    }
  )
)

ObservationTimeRange <- R6Class(
  classname = "ObservationTimeRange",
  inherit = Randomizable,
  private = list(
    .full_interval = lubridate::interval(),
    .randomize = function() {
      time_scale <- Scale$new()$value
      nperiods <- ceiling(as.numeric(lubridate::as.period(private$.full_interval, unit = time_scale), unit = time_scale))
      periods <- sort(sample(seq_len(nperiods), 2, replace = TRUE)) - 1
      start_date <- lubridate::int_start(private$.full_interval) + lubridate::period(periods[1], time_scale)
      end_date <- lubridate::int_start(private$.full_interval) + lubridate::period(periods[2] + 1, time_scale) - 1
      start_date <- max(start_date, lubridate::int_start(private$.full_interval))
      end_date <- min(end_date, lubridate::int_end(private$.full_interval))
      return(list(
        start_date = as.character(as.Date(start_date)),
        end_date = as.character(as.Date(end_date)),
        template = ObservationTemplate$new()$value
      ))
    }
  ),
  public = list(
    initialize = function(start, end) {
      private$.full_interval <- lubridate::interval(start, end + 1)
    }
  )
)

ObservationTimeRanges <- R6Class(
  classname = "ObservationTimeRanges",
  inherit = Randomizable,
  private = list(
    .start = lubridate::ymd(),
    .end = lubridate::ymd(),
    .randomize = function() {
      nobservations <- PositiveInteger$new()$value
      return(lapply(seq_len(nobservations), function(x) {
        ObservationTimeRange$new(private$.start, private$.end)$value
      }))
    }
  ),
  public = list(
    initialize = function(start, end) {
      private$.start <- start
      private$.end <- end
    }
  )
)

#' @export
run_test_case <- function(start_date = Date$new(),
                          duration = Period$new(),
                          time_scale = Scale$new(),
                          grid_size = PositiveInteger$new(),
                          polygon_type = PolygonTemplate$new(),
                          covariates = CovariateList$new(),
                          observations = ObservationTimeRanges$new(start = start_date$value, end = start_date$value + duration$value - 1),
                          processing = list(),
                          stan_parameters = list(niter = 1000, nchain = 2, model = "dagar_seasonal_flexible.stan")) {
  config <- list(
    general = list(
      location_name = "1",
      start_date = as.character(start_date$value),
      end_date = as.character(start_date$value + duration$value - 1),
      time_scale = time_scale$value,
      width_in_km = grid_size$value,
      height_in_km = grid_size$value,
      covariates = lapply(covariates$value[sapply(covariates$value, function(covariate) {
        covariate[["include_in_model"]]
      })], function(covariate) {
        return(list(
          name = covariate[["name"]]
        ))
      })
    ),
    processing = processing,
    stan = stan_parameters,
    test_metadata = list(
      raster = list(
        units = "years"
      ),
      polygons = list(
        template = polygon_type$value
      ),
      covariates = covariates$value,
      observations = observations$value
    )
  )

  Sys.setenv(CHOLERA_CONFIG = paste0(tempfile(), ".yml"))
  yaml::write_yaml(x = config, file = Sys.getenv("CHOLERA_CONFIG"))

  print("HERE")
  source(rprojroot::find_root_file(criterion = ".choldir", "Analysis", "R", "test_case", "test_case.R"))

  return(list(
    start_date = start_date,
    duration = duration,
    time_scale = time_scale,
    grid_size = grid_size,
    polygon_type = polygon_type,
    covariates = covariates,
    observations = observations,
    stan_parameters = stan_parameters
  ))
}
