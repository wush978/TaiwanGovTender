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
if (.rank == 0) {
  all_dir <- dir("tenders", "gz$", recursive = TRUE, full.names = TRUE)
  all_dir <- head(all_dir, 50)
  loginfo(sprintf("There are total %d files to process...", length(all_dir)))
  invisible(bcast(all_dir))
} else {
  all_dir <- bcast(character(0))
  loginfo(sprintf("Receiving total %d files to process...", length(all_dir)))
}
jid <- pbdMPI::get.jid(length(all_dir))
loginfo(sprintf("I have %d files to process", length(jid)))
retval <- list()
for(.i in seq_along(jid)) {
  d <- all_dir[jid[.i]]
  tryCatch({
    retval[[d]] <- get_content(d)
  }, error = function(e) {
    logerror(sprintf("Error is encoutered when I am processing %s ...", d))
    logerror(sprintf("The error message is: %s", conditionMessage(e)))
    quit("no", status = 1)
  })
  if (.i %% 100 == 0) {
    loginfo(sprintf("Progress: (%d/%d)", .i, length(jid)))
  }
}
saveRDS(retval, file = sprintf("parallel_parse_tenders-%03d.Rds", .rank))
pbdMPI::finalize()