noiseTypes <- c("Gauss", "Beta")
noiseParamsIdxs <- list(Gauss=1:9, Beta=1:19)
repIdxRanges <- lapply(1:100, \(x) c(1 + 10*(x-1), 10*x))

for (repIdxRange in repIdxRanges) for (noiseType in noiseTypes) for (noiseParamsIdx in noiseParamsIdxs[[noiseType]])  {
#for (repIdxRange in repIdxRanges[1:2]) for (noiseType in noiseTypes[1:2]) for (noiseParamsIdx in noiseParamsIdxs[[noiseType]][1:2])  {
  command <- sprintf("sbatch --qos=short --job-name=ELMgrid --output=_log/ELMgrid_%%j.out --error=_log/ELMgrid_%%j.err --time=1440 --cpus-per-task=1 --wrap=\"Rscript estimGridSearch1.R noiseType=\\'%s\\' noiseParamsIdx=%d repIdx=%d:%d\"", noiseType, noiseParamsIdx, repIdxRange[1], repIdxRange[2])
  cat(command, "\n")
  output <- system(command, intern = TRUE)
  cat(output, "\n")
}
