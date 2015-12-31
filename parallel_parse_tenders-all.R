suppressPackageStartupMessages({
  library(pbdMPI)
  library(logging)
  source("bparser.R")
  library(pbdMPILogging)
  basicConfig()
  convert_to_parallel_logger(getLogger())
})
.rank <- comm.rank()
.size <- comm.size()
retval.path <- sprintf("parallel_parse_tenders-%03d.Rds", .rank)
loginfo(sprintf("Reading archive %s ...", retval.path))
retval <- readRDS(retval.path)
retval.all <- gather(retval, unlist = FALSE)
finalize()
if (.rank == 0) {
  retval.all <- unlist(retval.all, recursive = FALSE)
  saveRDS(retval.all, "parallel_parse_tenders-all.Rds")
}
