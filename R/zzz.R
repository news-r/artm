artm <- NULL

.onLoad <- function(libname, pkgname) {
  artm <<- reticulate::import("artm", delay_load = TRUE, convert = FALSE)
}