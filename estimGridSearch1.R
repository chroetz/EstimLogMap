args <- commandArgs(TRUE)



noiseType <- "Beta" # Beta, Gauss
ns <- 2^(2:8)
noiseParamsIdx <- 1:1000
repIdx <- c(2,6)#1:1000



if (length(args > 0)) eval(parse(text = paste(args, collapse=";"))) # evaluate cmd args as R code



source("common.R")



dataFileName <- sprintf("LogMapData_%s.RDS", noiseType)
outputFileNamePattern <- sprintf("errorGs1_%s_%%s.RDS", noiseType)
data <- readRDS(file.path(.DataPath, dataFileName))
nReps <- length(repIdx)
noiseParams <- data$settings$noiseParams
noiseParamsIdx <- intersect(seq_along(noiseParams), noiseParamsIdx)
nParts <- 10
gridDataFileName <- "LogMapGrid_256_1000_10_%d.RDS"



estimateNs <- function(zObs, ns) {
  nMax <- ncol(zObs)

  resParts <- array(NA_real_, dim=c(nParts, nReps, 3, length(ns)))

  for (iPart in seq_len(nParts)) {
    pt <- proc.time()
    gridData <- readRDS(file.path(.LogMapGridStorePath, sprintf(gridDataFileName, iPart)))
    nMaxGrid <- min(c(ncol(gridData$z), nMax, max(ns)))
    gridDataZ <- gridData$z[, seq_len(nMaxGrid)]
    cat("Part:" , iPart, "with", nReps, "reps\n")
    for (iRepIdx in seq_along(repIdx)) {
      iRep <- repIdx[iRepIdx]
      cat(iRep, ", ")
      z <- zObs[iRep,]
      sqrDiff <- (gridDataZ - rep(z[seq_len(nMaxGrid)], each = nrow(gridData$z)))^2
      errors <- sapply(ns, \(n) rowSums(sqrDiff[,seq_len(min(c(n, nMaxGrid))), drop=FALSE]))
      minIdx <- apply(errors, 2, which.min)
      resParts[iPart, iRepIdx, , ] <- sapply(seq_along(minIdx), \(n) c(errors[minIdx[n],n], gridData$grid$z0[minIdx[n]], gridData$grid$r[minIdx[n]]))
    }
    cat(" took", (proc.time() - pt)[3], "s\n")
  }

  resPartsErr <- resParts[,,1,]
  dim(resPartsErr) <- dim(resParts)[c(1,2,4)]
  minPartIdxs <- apply(resPartsErr, c(2,3), which.min)
  res <- array(NA_real_, dim=c(nReps, length(ns), 3))
  for (iRepIdx in seq_along(repIdx)) for (nIdx in seq_along(ns)) {
    res[iRepIdx, nIdx, ] <- resParts[minPartIdxs[iRepIdx, nIdx], iRepIdx, , nIdx]
  }

  rEsti <- res[,,3]

  return(rEsti)
}

rEstis <- array(
  NA_real_,
  dim = c(nReps, length(ns), length(noiseParamsIdx)),
  dimnames = list(reps=seq_len(nReps), ns = ns, noiseParam = noiseParams[noiseParamsIdx])
)

for (i in seq_along(noiseParamsIdx)) {
  cat("NoiseIdx:", i, "\n")
  rEstis[ ,,i] <- estimateNs(data$noisy[[noiseParamsIdx[[i]]]], ns)
}

#rErr <- abs(rEstis - rep(data$r[seq_len(nReps)], times = length(noiseParamsIdx)*length(ns)))

# results <-
#   left_join(
#     rErr |>
#       apply(2:3, mean) |>
#       as_tibble(rownames = "n") |>
#       pivot_longer(-n, names_to="noiseParam", values_to="AE_mean") |>
#       mutate(n = as.integer(n), noiseParam = as.double(noiseParam)),
#     rErr |>
#       apply(2:3, sd) |>
#       as_tibble(rownames = "n") |>
#       pivot_longer(-n, names_to="noiseParam", values_to="AE_sd") |>
#       mutate(n = as.integer(n), noiseParam = as.double(noiseParam)),
#     join_by(n, noiseParam))

resultsList <- lst(rEstis, noiseType, ns, noiseParamsIdx, repIdx)
outputFileName <- sprintf(outputFileNamePattern, rlang::hash(resultsList))
write_rds(resultsList, file=file.path(.DataPath, outputFileName))
#write_csv(results, file.path(.DataPath, outputFileName))

