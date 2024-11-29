args <- commandArgs(TRUE)



noiseType <- "Gauss" # Beta, Gauss
ns <- 2^(2:8)
noiseParamsIdx <- 1:1000



if (length(args > 0)) eval(parse(text = paste(args, collapse=";"))) # evaluate cmd args as R code



source("common.R")



dataFileName <- sprintf("LogMapData_%s.RDS", noiseType)
outputFileName <- sprintf("errorOls_%s.csv", noiseType)
data <- readRDS(file.path(.DataPath, dataFileName))
nMax <- data$settings$nMax
nReps <- data$settings$nReps
noiseParams <- data$settings$noiseParams
noiseParamsIdx <- intersect(seq_along(noiseParams), noiseParamsIdx)



estimate <- function(z, n) {
  x <- z[, (nMax-n+1):(nMax-1), drop=FALSE]
  y <- z[, (nMax-n+2):(nMax), drop=FALSE]
  numerator <- y*x*(1-x)
  denominator <- x^2*(1-x)^2
  rEsti <- rowSums(numerator) / rowSums(denominator)
  return(rEsti)
}

rEstis <- array(
  NA_real_,
  dim = c(nReps, length(ns), length(noiseParamsIdx)),
  dimnames = list(reps=seq_len(nReps), ns = ns, noiseParam = noiseParams[noiseParamsIdx])
)

for (i in seq_along(noiseParamsIdx)) for (j in seq_along(ns)) {
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

