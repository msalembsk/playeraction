## nolint start
socceraction_py <- NULL

.load_dummy <- function() {
    test <- try(socceraction_py$spadl$config$actiontypes, silent = TRUE)
    check <- !is.null(test) && is.character(test)
    n_test <- 0
    while(!check & n_test <= 10) {
        socceraction_py <<- try(reticulate::import("socceraction2", delay_load = TRUE),
                                silent = FALSE)
        test <- try(socceraction_py$spadl$config$actiontypes, silent = TRUE)
        check <- !is.null(test) && is.character(test)
        n_test <- n_test + 1
    }

    check
}

## nolint start
.onLoad	<- function(libname, pkgname) {
    ## delay load foo module (will only be loaded when accessed via $)
    ##
    ## 'delay_load = TRUE' does not actually import the module and so may not
    ##  raise an error even if it is not really successful. In that case the
    ##  expressions in the 'if' statement below may silently be skipped even if
    ##  something has gone amis. (I didn't realise that I have no python or
    ##  conda for some time.)
    socceraction_py <<- try(reticulate::import("socceraction2", delay_load = TRUE),
                            silent = FALSE)
    .load_dummy()

    ## setting 'silent' to TRUE in non-interactive mode to hide a confusing
    ##     error message during installation of the package when there is no
    ##     running mongodb server. (the message is confusing because the
    ##     installation succeeds)
    ##
    ## Note that other messages printed below are suppressed by 'R CMD INSTALL'
    ##     and so don't cause confusion.
    val <- try(set_db(), silent = !interactive())
    if (inherits(val, "try-error")) {
        condmes <- conditionMessage(attr(val, "condition"))
        ## print the error message if it has not been printed yet (see above)
        if (!interactive())
            packageStartupMessage(condmes)

        ## This check is based ons manual inspection of the error message.
        ## Maybe it is setDdbAll() that should do this check?
        if (grepl("No suitable servers found", condmes)) {
            packageStartupMessage(
                paste("It seems that there is no running mongodb server.",
                      "Please start one.")
            )

            if (interactive())
                packageStartupMessage(
                    paste("Then set the database root with 'set_db()'",
                          "or reload the package.")
                )
        } else
            packageStartupMessage(
                paste("Please set the database root", "see 'set_db()'.")
            )
    } else
        packageStartupMessage("Database  set up properly !")
}
## nolint end
