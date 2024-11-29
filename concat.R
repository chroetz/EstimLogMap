source("common.R")

files <- list.files("~/EstimLogMapData/Gs1_RDS", pattern="^errorGs1_.*\\.RDS$", full.names=TRUE)
dataGauss <- readRDS(file.path(.DataPath, sprintf("LogMapData_%s.RDS", "Gauss")))
dataBeta <- readRDS(file.path(.DataPath, sprintf("LogMapData_%s.RDS", "Beta")))
rErrBeta <- array(NA_real_, dim=c(1000, 7, 19), dimnames=list(rep=1:1000, n=2^(2:8), noiseParam = dataBeta$settings$noiseParams))
rErrGauss <- array(NA_real_, dim=c(1000, 7, 9), dimnames=list(rep=1:1000, n=2^(2:8), noiseParam = dataGauss$settings$noiseParams))
for (file in files) {
  res <- read_rds(file)
  if (res$noiseType == "Gauss") {
    data <- dataGauss
  } else if (res$noiseType == "Beta") {
    data <- dataBeta
  } else {
    stop(res$noiseType)
  }
  rErr <- abs(res$rEstis - rep(data$r[res$repIdx], times = length(res$noiseParamsIdx)*length(res$ns)))
  if (res$noiseType == "Gauss") {
    rErrGauss[res$repIdx, , res$noiseParamsIdx] <- rErr
  } else if (res$noiseType == "Beta") {
    rErrBeta[res$repIdx, , res$noiseParamsIdx] <- rErr
  } else {
    stop(res$noiseType)
  }
}
sum(is.na(rErrBeta))
sum(is.na(rErrGauss))


rErr <- rErrBeta
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
noiseType <- "Beta"
outputFileName <- sprintf("errorGs1_%s.csv", noiseType)
write_csv(results, file.path(.DataPath, outputFileName))

rErr <- rErrGauss
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
noiseType <- "Gauss"
outputFileName <- sprintf("errorGs1_%s.csv", noiseType)
write_csv(results, file.path(.DataPath, outputFileName))
