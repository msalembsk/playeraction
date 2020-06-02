## nolint start
.onAttach <- function(libname, pkgname) {
  val <- try(set_db_all(), silent = TRUE)
  if (inherits(val, "try-error")) {
    packageStartupMessage("Please set the database root, see 'setDbAll()' !")
  } else {
    packageStartupMessage("Package settings set up properly playeraction !")
  }
}
## nolint end
