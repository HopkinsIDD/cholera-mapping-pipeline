options(error = function(...) {
  quit(..., status = 2)
})
print("Running tests")
install.packages("../packages/taxdat", type = "source", repos = NULL)
source("../Analysis/R/set_parameters.R")
