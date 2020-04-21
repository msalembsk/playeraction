## nolint start
.onAttach <- function(libname, pkgname) {
  val <- tryLog(set_db_all())
  if (inherits(val, "try-error")) {
    packageStartupMessage("Please set the database root, see 'setDbAll()' !")
  } else {
    packageStartupMessage("Settings set up properly playeraction !")
  }
}
## nolint end
