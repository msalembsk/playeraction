#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom utils head tail
#' @importFrom zeallot %<-%
NULL

## holding place for settings
.settings <- new.env()

#' set up the package settings
#'
#' set up the package settings
#'
#' save the package settings in a top-level environment called
#' \code{.settings}. The function is called at loading time with default
#' arguments.
#'
#' @param json_config_file_name,json_config_path,json_config_root
#'     character info needed to access the global config file.
#' @param database_type character type of data-base connection
#' @param target_db character type of database playeraction
#' @param collections_provider character name of collection to connect to from
#'     the \code{data_provider} family
#' @param collections_target character name of collection to connect to from
#'     the \code{target_db} family
#' @param project_name character project-name
#' @import tryCatchLog futile.logger tidyverse aroundthegoal R6
#' @importFrom purrr map
#' @export
set_db_all <- function(json_config_file_name = "config_global_ra.json",
                       json_config_path = "secrets",
                       json_config_root = "",
                       data_provider = c("opta", "inStat",
                                         "STATS", "whoScored"),
                       database_type = c("localhost", "prod", "prod_backup"),
                       ## useful collections
                       feed_collections = c("events", "fixtures", "players"),
                       spadl_collections = c("events", "features",
                                             "action_values"),
                       spadl_config = "spadl_config.json",
                       project_name = "playeraction") {

    ## ========================= json config
    json_config <- read_internal_data(file = json_config_file_name,
                                      path = json_config_path,
                                      root = json_config_root,
                                      pkg_name = "playeraction")

    .settings$spadl_config <- read_internal_data(
        file = spadl_config,
        path = file.path("inst", "extdata"),
        pkg_name = "playeraction"
    )

    ## ========================= mongo connections
    ## get the appropriate database
    database <- match.arg(database_type)
    data_provider <- match.arg(data_provider)
    if (!data_provider %in% json_config$dataProviders) {
        flog.error(paste("data-provider:", data_provider, "not recognised !"))
        stop()
    }

    .mongo_con <- function(name, family, use_family = FALSE) {
        con_name <- paste0(name, "_con")
        if (use_family)
            con_name <- paste0(family, "_", con_name)

        .settings[[con_name]] <-
            mongo_connect_from_config(ljson_config = json_config,
                                      family = family,
                                      name = name,
                                      data_provider = data_provider,
                                      category = "collections",
                                      sub_name = NULL,
                                      database = database)
    }

    ## feed target
    map(feed_collections, .mongo_con, family = "feed")

    ## features
    map(spadl_collections, .mongo_con, family = "spadl")

    ## info log file
    set_up_logger(ljson_config = json_config, project_name = project_name)

    ls(.settings, all.names = TRUE)
}
