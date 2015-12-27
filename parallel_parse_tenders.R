suppressPackageStartupMessages({
  library(pbdMPI)
  .rank <- comm.rank()
  .size <- comm.size()
  source("bparser.R")
})
basicConfig()
if (.rank == 0) {
  all_dir <- dir("./", "gz$", recursive = TRUE)
  bcast(all_dir)
} else {
  all_dir <- bcast(character(0))
}
