if (dir.exists("/p/projects/ou/labs/ai/DEEB")) {
  .RootDir <- "/p/projects/ou/labs/ai/DEEB"
} else {
  .RootDir <- "~"
}
.DataPath <- file.path(.RootDir, "EstimLogMapData")
.LogMapGridStorePath <- file.path(.DataPath, "LogMapGrid")

library(tidyverse)
