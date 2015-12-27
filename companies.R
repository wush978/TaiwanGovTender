library(magrittr)
library(jsonlite)
library(logging)
basicConfig()

library(data.table)

companies <- 
  local({
    dst <- "company-info.Rds"
    parse_company <- function(path) {
      dst <- file.path(dirname(path), sub(pattern = "json", replacement = "Rds", basename(path)))
      if (file.exists(dst)) {
        loginfo(sprintf("Loading RDS of %s...", path))
        return(readRDS(dst))
      } 
      loginfo(sprintf("Parsing %s ...", path))
      tmp <- readLines(path)
      tmp2 <- regmatches(tmp, regexec("^(\\d+),(.*)$", tmp))
      tmp3 <-
        lapply(tmp2, `[`, 3) %>%
        lapply(fromJSON)
      tmp4 <-
        lapply(tmp3, `[[`, "董監事名單") %>% 
        sapply(function(df) {
          if (is.null(nrow(df))) {
            ""
          } else {
            paste(df[["姓名"]], collapse = ",")
          }
        })
      loginfo(sprintf("Generating data.frame ..."))
      retval <- data.frame(stringsAsFactors = FALSE,
                 id = sapply(tmp2, `[`, 2),
                 magnate = tmp4)
      loginfo(sprintf("Writing Rds..."))
      saveRDS(object = retval, file = dst)
      retval
    }
    if (file.exists(dst)) {
      loginfo(sprintf("Loading company-info..."))
      readRDS(dst)
    } else {
      companies <-
        dir("company-info", "^\\d*.json", full.names = TRUE) %>%
        lapply(parse_company)
      
      loginfo("Combining data.frame...")
      companies <- rbindlist(companies)
      loginfo("Writing result...")
      saveRDS(companies, dst)
      companies
    }
  })
# object.size(companies)
