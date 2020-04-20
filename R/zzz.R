.onAttach <- function(libname, pkgname) {
  val <- tryLog(setDbAll())
  if (inherits(val, "try-error")) {
    packageStartupMessage("Please set the database root, see 'setDbAll()' !")
  } else {
    packageStartupMessage("Settings set up properly playeraction !")
  }
}
