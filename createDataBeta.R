
seed <- 0
nReps <- 1000
nMax <- 256
precision <- 2^(-2:16) # Beta noise model
rRange <- c(3.57, 4) # chaotic regime
outputFileName <- "LogMapData_Beta.RDS"


source("common.R")

set.seed(seed)

getTimeSeries <- function(n, z0, map, param) {
  Z <- matrix(NA_real_, nrow = length(z0), ncol = n)
  Z[, 1] <- z0
  for (i in seq_len(n-1)+1) Z[, i] <- map(Z[, i-1], param)
  return(Z)
}

logisticMap <- \(z, param) param*z*(1-z)

r <- runif(nReps, min=rRange[1], max=rRange[2])
z0 <- runif(nReps)

z <- getTimeSeries(nMax, z0, logisticMap, r)

zNoisedList <- lapply(precision, \(prec) {
  alpha <- as.vector(z * prec)
  beta <- as.vector((1 - z) * prec)
  matrix(rbeta(length(alpha), shape1 = alpha, shape2 = beta), nrow = nrow(z), ncol = ncol(z))
})

data <- list(
  settings = dplyr::lst(seed, nReps, nMax, precision, rRange, noiseType = "Beta", noiseParams = precision),
  r = r,
  z0 = z0,
  noisy = zNoisedList,
  truth = z)

saveRDS(data, file.path(.DataPath, outputFileName))

