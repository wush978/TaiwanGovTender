pkg_list <- c(
  "optparse",
  "XML",
  "httr",
  "magrittr",
  "dplyr",
  "lubridate",
  "logging"
)

for(pkg in pkg_list) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
  }
}