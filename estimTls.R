args <- commandArgs(TRUE)



noiseType <- "Beta" # Beta, Gauss
ns <- 2^(2:8)
noiseParamsIdx <- 1:1000



if (length(args > 0)) eval(parse(text = paste(args, collapse=";"))) # evaluate cmd args as R code



source("common.R")



dataFileName <- sprintf("LogMapData_%s.RDS", noiseType)
outputFileName <- sprintf("errorTls_%s.csv", noiseType)
data <- readRDS(file.path(.DataPath, dataFileName))
nMax <- data$settings$nMax
nReps <- data$settings$nReps
noiseParams <- data$settings$noiseParams
noiseParamsIdx <- intersect(seq_along(noiseParams), noiseParamsIdx)



complexRoots <- function(a,b,c,q,p) {
  w0 <- as.complex(2 * a * q + b)
  u0 <- as.complex(4 * a * (c - p) - b^2 + 2)
  v0 <- 54 * a^3 * w0
  u1 <- 108 * a^6 * u0^3
  u2 <- sqrt(as.complex(9^2 * w0^2 + 3 * u0^3))
  u4 <- (54 * a^3 * w0 + 6 * abs(a^3) * u2)^(1/3)
  r1 <-
    u4 / (6 * 2^(1/3) * a^2) -
    u0 / (2^(2/3) * u4) -
    b / (2 * a)
  wa <- v0 + sqrt((v0)^2 + 4*(3*a^2*u0)^3)
  r2 <-
    -(1 - 1i*sqrt(3))*wa^(1/3)/(12*2^(1/3)*a^2) +
    (1 + 1i*sqrt(3))*(3*a^2*u0)/(6*2^(2/3)*a^2*wa^(1/3)) -
    b/(2*a)
  r3 <-
    -(1 + 1i*sqrt(3))*wa^(1/3)/(12*2^(1/3)*a^2) +
    (1 - 1i*sqrt(3))*(3*a^2*u0)/(6*2^(2/3)*a^2*wa^(1/3)) -
    b/(2*a)
  r <- c(r1, r2, r3)
  r
}

realRoot <- function(a,b,c,q,p) {
  r <- complexRoots(a,b,c,q,p)
  realRoots <- sort(Re(r[abs(Im(r)) < 1e-10]))
  parab <- \(x, a, b, c) a*x^2 + b*x + c
  if (length(realRoots) > 1) {
    err <- (realRoots - q)^2 + (parab(realRoots, a, b, c) - p)^2
    realRoots <- realRoots[which.min(err)]
  }
  if (length(realRoots) != 1) {
    # Fallback
    cat("-Fallback- ")
    res <- optimize(\(r) (r - q)^2 + (parab(r, a, b, c) - p)^2, interval=c(0,1), tol=1e-6)
    realRoots <- res$minimum
  }
  realRoots
}

se <- function(x, y, a, b, c) {
  n <- length(x)
  err <- 0
  for (i in 1:n) {
    xParab <- realRoot(a, b, c, x[i], y[i])
    err <- err + (xParab - x[i])^2 + (a*xParab^2 + b*xParab + c - y[i])^2
  }
  return(err)
}

estimate <- function(z, n) {
  x <- z[, (nMax-n+1):(nMax-1), drop=FALSE]
  y <- z[, (nMax-n+2):(nMax), drop=FALSE]
  rEsti <- sapply(seq_len(nReps), \(k) {
    res <- optimize(\(r) se(x[k,], y[k,], -r, r, 0), interval=c(3,4), tol=1e-6)
    res$minimum
  })
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

