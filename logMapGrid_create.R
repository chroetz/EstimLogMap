args <- commandArgs(TRUE)

source("common.R")

n <- 2^8
nGrid <- 1000
nParts <- 10
iPart <- 2

if (length(args > 0)) eval(parse(text = paste(args, collapse=";"))) # evaluate cmd args as R code

getTimeSeries <- function(n, z0, map, param) {
  Z <- matrix(NA_real_, nrow = length(z0), ncol = n)
  Z[, 1] <- z0
  for (i in seq_len(n-1)+1) Z[, i] <- map(Z[, i-1], param)
  return(Z)
}

logisticMap <- \(z, param) param*z*(1-z)

z0 <- seq(0, 1, len=nGrid)
rAll <- seq(3, 4, len=nGrid)
if (nParts == 1) {
  partIdx <- rep(1, length(rAll))
} else {
  partIdx <- cut(rAll, nParts, labels=FALSE)
}
r <- rAll[partIdx == iPart]

grid <- expand.grid(z0 = z0, r = r)
z <- getTimeSeries(n, grid$z0, logisticMap, grid$r)

resList <- dplyr::lst(grid, z, z0, rAll, iPart, r)
saveRDS(
  resList,
  file = file.path(
    .LogMapGridStorePath,
    sprintf("LogMapGrid_%d_%d_%d_%d.RDS", n, nGrid, nParts, iPart)))
