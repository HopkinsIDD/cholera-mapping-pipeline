
# function that makes distance matrix for a side*side 2D array  
dist.matrix <- function(side)
{
  row.coords <- rep(1:side, times=side)
  col.coords <- rep(1:side, each=side)
  row.col <- data.frame(row.coords, col.coords)
  D <- dist(row.col, method="euclidean", diag=TRUE, upper=TRUE)
  D <- as.matrix(D)
  return(D)
}

# function that simulates the autocorrelated 2D array with a given side,
# and with exponential decay given by lambda
# (the mean mu is constant over the array, it equals to global.mu)
cor.surface <- function(side, global.mu, lambda)
{
  D <- dist.matrix(side)
  # scaling the distance matrix by the exponential decay
  SIGMA <- exp(-lambda*D)
  mu <- rep(global.mu, times=side*side)
  # sampling from the multivariate normal distribution
  M <- matrix(nrow=side, ncol=side)
  M[] <- rmvnorm(1, mu, SIGMA)
  return(M)
}

#### function to generate random spatial effects given an adjacency matrix
cor.surface <- function(side, global.mu, lambda)
{
  D <- dist.matrix(side)
  # scaling the distance matrix by the exponential decay
  SIGMA <- exp(-lambda*D)
  mu <- rep(global.mu, times=side*side)
  # sampling from the multivariate normal distribution
  M <- matrix(nrow=side, ncol=side)
  M[] <- rmvnorm(1, mu, SIGMA)
  return(M)
}

## Generate grid with 
#sim.g <- grf(grid = expand.grid(x = seq(1, 1000, l = 1000),y = seq(1, 1000, l = 1000)), cov.pars = c(0.1, 0.2))


  
