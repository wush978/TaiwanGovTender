library(magrittr)
library(optparse)
opt <- parse_args(OptionParser(option_list = list(
  make_option("--month", type = "character", default = format(Sys.Date() - 30, "%Y-%m"))
)))

page_csv_list <- dir(opt$month, pattern = "^\\d{5}-page.csv.gz$", full.names = TRUE)

library(logging)
basicConfig()

parse_href <- function(href) {
  get_subpattern <- function(pattern, x) {
    m <- regexec(pattern, x)
    regmatches(x, m)[[1]][2]
  }
  get_dir <- function(text, len) {
    retval <- substring(text, seq(1, nchar(text)-len+1, len), seq(len, nchar(text), len))
    if (sum(nchar(retval)) < nchar(text)) {
      c(retval, substring(text, sum(nchar(retval)) + 1, nchar(text)))
    } else retval
  }
  pkAtmMain <- get_subpattern("pkAtmMain=(\\d+)&", href) %>% as.integer
  tenderCaseNo <- get_subpattern("tenderCaseNo=(.*)$", href)
  fname <- paste(tenderCaseNo, "html", sep = ".")
  file.path(dst_root, do.call(file.path, get_dir(pkAtmMain, 3) %>% as.list), fname)
}

dst_root <- "tenders"

library(httr)

browse_html <- function(gz_path) {
  tmp.path <- tempfile(fileext = ".html")
  R.utils::gunzip(gz_path, destname = tmp.path, remove = FALSE)
  browseURL(tmp.path)
  invisible(NULL)
}

for(i in seq_along(page_csv_list)) {
  page_csv_path <- page_csv_list[i]
  loginfo(sprintf("Crawling page from %s", page_csv_path))
  df <- read.table(gzfile(page_csv_path), header = TRUE, sep = ",", colClasses = "character")
  for(j in seq_len(nrow(df))) {
    loginfo(sprintf("\tCrawling (%d/%d)", j, nrow(df)))
    href <- df$href[j]
    dst_path <- parse_href(href)
    if (!file.exists(dst_path)) {
      if (!dirname(dst_path) %>% dir.exists) dir.create(dirname(dst_path), recursive = TRUE)
      url <- gsub("..", "http://web.pcc.gov.tw/tps", href, fixed = TRUE)
      res <- GET(url)
      stop_for_status(res)
      writeBin(content(res, "raw"), dst_path)
      R.utils::gzip(dst_path)
      # browse_html(paste(dst_path, "gz", sep = "."))
      Sys.sleep(rpois(1, 1))
    }
  }
}
