options(repos = "https://cran.stat.auckland.ac.nz/")
options(warnPartialMatchDollar = TRUE)
if (interactive() && stringr::str_detect(getwd(), "Packages")) {
  suppressMessages(require(devtools))
}

.pkg_name <- "datr"
reinstall <- function() {
  if (.pkg_name %in% installed.packages()) {
    remove.packages(.pkg_name, lib = "/Users/josh/.rlibs/common/4.1")
  }
  devtools::document()
  devtools::install()
}
