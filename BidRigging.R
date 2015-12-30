library(magrittr)
library(data.table)
companies <- readRDS("company-info.Rds") %>%
  data.table
setkey(companies, "id")

# BidRigging
tenders <- readRDS("tenders.Rds")
tenders.id <- 
  tenders %>% 
  lapply(`[[`, "tender_company") %>%
  lapply(sapply, `[[`, "id")

tender.magnate <- 
  tenders.id %>% # head(1000) %>%
  lapply(function(x) {
    id.is_valid <- grepl("^\\d+$", x)
    retval <- character(length(x))
    retval[id.is_valid] <- companies[x[id.is_valid]]$magnate
    retval
  })

## Global Intersection / Union

magnate.score <- 
  tender.magnate %>%
  Filter(f = function(x) length(x) > 1) %>%
  sapply(function(x) {
    x[is.na(x)] <- ""
    x <- gsub("\\s", "", x)
    x.list <- strsplit(x, ",")
    a <- Reduce(intersect, x.list) %>% length
    b <- Reduce(union, x.list) %>% length
    if (b == 0) 0 else a / b
  })

suspects.name <- which(magnate.score > 1/3 - 1e-5) %>% names
tender.magnate[suspects.name]
tenders[suspects.name][[2]]
