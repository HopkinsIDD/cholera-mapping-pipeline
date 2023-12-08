home <- Sys.getenv("HOME")

user_lib <- paste0(home, "/R/x86_64-pc-linux-gnu-library/4.2")
# We set personal library path as the default one
.libPaths(c(user_lib, "/data/apps/extern/easybuild/R/4.2.1-foss-2022a/lib64/R/library"))
options(repos = c(
  CRAN = "https://cran.r-project.org",
  CMDSTANR = "https://mc-stan.org/r-packages/"
))