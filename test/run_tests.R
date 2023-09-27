options(error = function(...) {
  quit(..., status = 2)
})
if (!dir.exists("../Analysis/data")) {
  dir.create("../Analysis/data")
}
if (!dir.exists("../Analysis/configs")) {
  dir.create("../Analysis/configs")
}
file.copy("config_dictionary.yml", "../Analysis/configs/config_dictionary.yml")
print("Running tests")
install.packages("../packages/taxdat", type = "source", repos = NULL)
source("../Analysis/R/set_parameters.R")
