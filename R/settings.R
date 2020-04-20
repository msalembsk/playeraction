## to deal with CRAN's 'no visible binding ...' when using %>%
##    use '.data$colname' instead of just 'colname'
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom utils head tail
#' @importFrom zeallot %<-%
NULL

## holding place for settings
.Settings <- new.env()

#' set up the package settings
#'
#' set up the package settings
#'
#' save the package settings in a top-level environment called
#' \code{.Settings}. The function is called at loading time with default
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
#' @import tryCatchLog futile.logger dplyr aroundthegoal
#' @export
setDbAll <- function(json_config_file_name = "config_global_ra.json",
                     json_config_path = "secrets",
                     json_config_root = "",
                     target_db = "playeraction",
                     database_type = c("local", "cloud"),
                     collections_provider = c("events"),
                     collections_target = c("actions", "actiontypes", "results",
                                            "bodyparts", "eventtypes"),
                     project_name = "playeraction") {


  ## ========================= json config
  json_config <- aroundthegoal::readInternalData(file = json_config_file_name,
                                                 path = json_config_path,
                                                 root = json_config_root,
                                                 pkgName = "playeraction")
  ## ========================= mongo connections
  ## get the appropriate database
  database_type <- match.arg(database_type)
  database <- switch(database_type,
                     "local" = "localhost",
                     "cloud" = "read")

  ## database provider
  data_provider <- json_config$projects[[project_name]]$data_provider

  .mongo_con <- function(name, family, db) {
    .Settings[[paste0(name, "Con")]] <-
      mongo_connect_from_config(ljson_config = json_config,
                                family = family, name = name,
                                data_provider = db,
                                category = "collections", sub_name = NULL,
                                database = database)
  }

  ## feed target
  lapply(collections_target, .mongo_con, family = target_db,
         db = target_db)

  ## feed provider (events)
  lapply(collections_provider, .mongo_con, family = data_provider,
         db = data_provider)


  ## info log file
  set_up_logger(ljson_config = json_config, project_name = project_name)

  ls(.Settings, all.names = TRUE)

}
