# --------------------------------------------------------------------------
# Purpose: functions to summarize suspected and confirmed cholera case data
# --------------------------------------------------------------------------

# Get region the data is coming from ----------------
get_region <- function(loc_name) {
  reg <- substr(loc_name, 1, 4)
  reg <- gsub("[^[:alnum:]=\\.]", "", reg)
  return(reg)
}

# Get approx geo level based on location name ----------------
get_geo_level <- function(loc_name) {
  if (is.na(loc_name)) {
    return(99)
  } else {
    colons <- str_count(loc_name, "::")
    return(colons - 1)
  }
}

# Get year group based on start date ----------------
get_year <- function(date) {
  year <- as.numeric(format(as.Date(date), "%Y"))
  return(year)
}

# Check whether cases were reported ----------------
cases_reported <- function(x) {
  reported <- ifelse(is.na(x), TRUE, FALSE)
  return(reported)
}
