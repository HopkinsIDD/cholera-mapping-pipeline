# -------------------------------------------------------------
# Purpose: summarize suspected and confirmed cholera case data
# -------------------------------------------------------------


# Set up ----------------------------------------------------------------

# clear environment
rm(list=ls())

# load packages
package_list <- c('taxdat', 'lubridate', 'stringr', 'data.table')
invisible(lapply(package_list, library, character.only = TRUE))

# load data functions
source('Analysis/R/notebooks/gtfcc_data_scoping/summarize_data_functions.R')

# set inputs
use_sql         <- TRUE
start_time      <- ymd('2010-01-01')
end_time        <- ymd('2020-10-31')
country_file    <- 'Analysis/R/notebooks/gtfcc_data_scoping/inputs/db_country_codes_20201026.csv'
locations_file  <- 'Analysis/R/notebooks/gtfcc_data_scoping/inputs/all_loctions_in_db_2020-10-11.csv'
key_file        <- 'Analysis/R/database_api_key.R'
save_dir        <- 'Analysis/R/notebooks/gtfcc_data_scoping/outputs/'
# -----------------------------------------------------------------------


# Load inputs ------------------------------------------------------

# load countries
countries_config <- fread(country_file)
if (use_sql) {
	id_col <- 'id'
	countries <- as.numeric(countries_config[[id_col]])
} else {
	id_col <- 'country_code'
	countries <- countries_config[[id_col]]
}

# load all location ids
all_locations <- fread(locations_file)

# load database login credentials and set source
source(key_file)
username <- ifelse(use_sql, taxonomy_username, database_username)
password <- ifelse(use_sql, taxonomy_password, database_api_key)
data_source <- ifelse(use_sql, 'sql',  'api')
# ------------------------------------------------------------------


# Function to load and clean data -------------------------------------------------

# Initiate function
summarize_case_data <- function(loc_id) {

	# wrapping in tryCatch so doesn't break the lapply if we don't have location 
	# observations in database, and instead returns the error message
	tryCatch({

		# report location information
		country_id <- countries_config[get(id_col) == loc_id, country]
		region_id <- countries_config[get(id_col) == loc_id, get_region(country_code)]
		message('\n', country_id, ' (', loc_id, ')')

		# pull case data by country
		cases <- pull_taxonomy_data(
		  username = username,
		  password = password,
		  locations = loc_id,
		  time_left = start_time,
		  time_right = end_time,
		  source = data_source,
		)

		# select required columns based on sql/api source
		time_col <- ifelse(use_sql, 'time_left', 'attributes.time_left')
		phantom_col <- ifelse(use_sql, 'phantom', 'attributes.phantom')
		primary_col <- ifelse(use_sql, 'primary', 'attributes.primary')
		confirmed_col <- ifelse(use_sql, 'confirmed_cases', 'attributes.fields.confirmed_cases') 
		suspected_col <- ifelse(use_sql, 'suspected_cases', 'attributes.fields.suspected_cases')
		loc_id_col <- ifelse(use_sql, 'location_id', 'attributes.location_id.x')
		loc_name_col <- ifelse(use_sql, 'location_name', 'attributes.location_name')

		# create simplified data table
		cases <- data.table(id = cases[['id']],
			                time_left = cases[[time_col]],
			                phantom = cases[[phantom_col]],
			                primary = cases[[primary_col]],
			                location_id = cases[[loc_id_col]],
			                location_name = cases[[loc_name_col]],
			                suspected_cases = cases[[suspected_col]],
			                confirmed_cases = cases[[confirmed_col]])
		
		# use location_id to set location_name where needed and is possible
		if (use_sql) {
			for (loc in unique(cases[is.na(location_name), location_id])) {
				lookup_loc <- all_locations[id == loc]
					if (nrow(lookup_loc) == 1) {
						cases[location_id == loc, location_name := lookup_loc$region]
				}
			}
		}
		
		# create needed columns
		cases[, country := country_id]
		cases[, region := region_id]
		cases[, geo_group := get_geo_level(location_name), by = 'id']
		cases[, year := get_year(time_left), by = 'id']
		cases[, suspected_report := ifelse(is.na(suspected_cases), FALSE, TRUE), by = 'id']
		cases[, confirmed_report := ifelse(is.na(confirmed_cases), FALSE, TRUE), by = 'id']
		cases[, suspected_zero := ifelse(suspected_cases == 0, TRUE, FALSE), by = 'id']
		cases[, confirmed_zero := ifelse(confirmed_cases == 0, TRUE, FALSE), by = 'id']

		# summarize data by country
		summary <- copy(cases)
		summary <- summary[, lapply(.SD, sum, na.rm = TRUE), 
							.SDcols = c('suspected_report', 'confirmed_report', 'suspected_zero', 'confirmed_zero'), 
							by = c('region', 'country', 'geo_group', 'year', 'phantom', 'primary')]
		setorderv(summary, c('region', 'country', 'geo_group', 'year', 'phantom', 'primary'))
		
		# return summary
		return(summary)

	# return the first 100 characters of the error message
	}, error = function(e) {
		err <- cat("ERROR :", substr(conditionMessage(e), 1, 100), "...\n")
		return(err)
	})
}
# ---------------------------------------------------------------------------------


# Apply function and save output ---------------------------------------------

# lapply across countries
dt1 <- rbindlist(lapply(countries, summarize_case_data),
                 use.names = T, fill = T)

# aggregate to region
dt2 <- copy(dt1)
dt2 <- dt2[, lapply(.SD, sum),
            .SDcols = c('suspected_report', 'confirmed_report', 'suspected_zero', 'confirmed_zero'), 
			by = c('region', 'geo_group', 'year', 'phantom', 'primary')]
setorderv(dt2, c('region', 'geo_group', 'year', 'phantom', 'primary'))

# save summaries
write.csv(dt1, paste0(save_dir, '/country_data_summary_', Sys.Date(), '.csv'))
write.csv(dt2, paste0(save_dir, '/region_data_summary_', Sys.Date(), '.csv'))
# ----------------------------------------------------------------------------
