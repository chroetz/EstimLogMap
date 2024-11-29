args <- commandArgs(TRUE)



noiseType <- "Beta" # Beta, Gauss
ns <- 2^(2:8)
noiseParamsIdx <- 1:1000



if (length(args > 0)) eval(parse(text = paste(args, collapse=";"))) # evaluate cmd args as R code



source("common.R")



dataFileName <- sprintf("LogMapData_%s.RDS", noiseType)
outputFileName <- sprintf("errorGs1_%s.csv", noiseType)
data <- readRDS(file.path(.DataPath, dataFileName))
nMax <- data$settings$nMax
nReps <- data$settings$nReps
noiseParams <- data$settings$noiseParams
noiseParamsIdx <- intersect(seq_along(noiseParams), noiseParamsIdx)



estimate <- function(z, ns) {
  nMax <- ncol(z)
  nReps <- nrow(z)

  nParts <- 100
  resParts <- array(NA_real_, dim=c(nParts, nReps, 3, length(ns)))

  for (iPart in seq_len(nParts)) {
    pt <- proc.time()
    gridData <- readRDS(file.path(.LogMapGridStorePath, sprintf("LogMapGrid_32_10000_100_%d.RDS", iPart)))
    cat(iPart)
    for (iRep in seq_len(nReps)) {
      z <- zObs[iRep,]
      sqrDiff <- (gridData$z[, seq_along(z)] - rep(z, each = nrow(gridData$z)))^2
      errors <- sapply(ns, \(n) rowSums(sqrDiff[,(nMax-n+1):nMax, drop=FALSE]))
      # TODO: check following
      minIdx <- apply(errors, 2, which.min)
      resParts[iPart, iRep, , ] <- sapply(seq_along(minIdx), \(n) c(errors[minIdx[n],n], gridData$grid$z0[minIdx[n]], gridData$grid$r[minIdx[n]]))
    }
    cat(" took", (proc.time() - pt)[3], "s\n")
  }

  minPartIdxs <- apply(resParts[,,1,], c(2,3), which.min)
  res <- array(NA_real_, dim=c(nReps, nMax, 3))
  for (iRep in seq_len(nReps)) for (n in seq_len(nMax)) {
    res[iRep, n, ] <- resParts[minPartIdxs[iRep, n], iRep, , n]
  }
  #TODO
  return(rEsti)
}

rEstis <- array(
  NA_real_,
  dim = c(nReps, length(ns), length(noiseParamsIdx)),
  dimnames = list(reps=seq_len(nReps), ns = ns, noiseParam = noiseParams[noiseParamsIdx])
)

for (i in seq_along(noiseParamsIdx)) for (j in seq_along(ns)) {
  cat(i, ", ", j, "; ")
  rEstis[ ,j,i] <- estimate(data$noisy[[noiseParamsIdx[[i]]]], ns[[j]])
}

rErr <- abs(rEstis - rep(data$r[seq_len(nReps)], times = length(noiseParamsIdx)*length(ns)))

results <-
  left_join(
    rErr |>
      apply(2:3, mean) |>
      as_tibble(rownames = "n") |>
      pivot_longer(-n, names_to="noiseParam", values_to="AE_mean") |>
      mutate(n = as.integer(n), noiseParam = as.double(noiseParam)),
    rErr |>
      apply(2:3, sd) |>
      as_tibble(rownames = "n") |>
      pivot_longer(-n, names_to="noiseParam", values_to="AE_sd") |>
      mutate(n = as.integer(n), noiseParam = as.double(noiseParam)),
    join_by(n, noiseParam))

write_csv(results, file.path(.DataPath, outputFileName))

