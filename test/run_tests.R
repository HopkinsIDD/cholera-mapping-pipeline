options(error = function(...) {
  quit(..., status = 2)
})
if (!dir.exists("../Analysis/data")) {
  dir.create("../Analysis/data")
}
print("Running tests")
install.packages("../packages/taxdat", type = "source", repos = NULL)
source("../Analysis/R/set_parameters.R")
