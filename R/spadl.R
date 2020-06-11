#' Translating raw data to SPADL representation
#'
#' We use SPADL common language (see details in the paper) to represent raw data
#' regardless of the data provider used. Every data provider has his own
#' implement class inheriting from the mother class.
#' @name Spadl
#' @import R6 tibble
Spadl = R6::R6Class("Spadl",
                    public = list(
                      #' @field data \code{tibble} :: the SPADL values stored as data.table
                      data = NULL,

                      #' @field metadata \code{tibble} :: metadata such as team name,
                      #' game-date, ... should have the same number of rows
                      #' as `data` and same game-ids.
                      metadata = NULL,

                      #' @description
                      #' Creates the 2 \code{tibble}(s) from mongo connections.
                      #' Given that the fixture collection is similar for all
                      #' data providers, the `metadata` field can be initialised here.
                      #' Must be implemented in each subclass (for each data provider).
                      #'
                      #' @param game_ids \code{integer}
                      #' @param fixture_con mongo-connection :: fixture collection
                      #' connection
                      initialize = function(game_ids, fixture_con) {
                          ## 1) metata initialisation here
                          keys <- list(gameId = game_ids)
                          qr <- buildQuery(names(keys), keys)
                          out <- list(gameId = 1, seasonId = 1, gameDate = 1,
                                      competitionId = 1, competitionName = 1,
                                      homeTeamId = 1, awayTeamId = 1,
                                      homeTeamName = 1, awayTeamName = 1,
                                      "_id" = 0)
                          qo <- buildQuery(names(out), out)
                          self$metadata <- fixture_con$find(qr, qo)
                          ## 2) data left as NULL
                      },

                      #' @description
                      #' Runs some sanity checks on the created instance
                      #' Regardless of the data provider (and the subclass used
                      #' to initialise the object), we expect the fields
                      #' values to be the same and hence sanity checks can be
                      #' implemented here.
                      #'
                      #' @return logical :: `TRUE` if all
                      #' tests are passed otherwise `FALSE` with an informative
                      #' error message
                      sanity_check = function() {
                        ## TODO
                      },

                      #' @description
                      #' Insert the instance in the mongo database if sanity-checks are successful
                      #'
                      #' @param mongo_con `mongo-connection` :: where the object should be inserted
                      #' @return logical :: insertion status
                      insert = function(mongo_con) {
                        ## TODO
                      },

                      #' @description
                      #' Converts the object to a \code{tibble}
                      #'
                      #' We simply join the \code{metadata} and \code{data}
                      #' fields by \code{gameId}.
                      #' @return \code{tibble}
                      as_table = function() {
                        left_join(self$metdata, self$data, by = "gameId")
                      }
                    ),
                    private = list(
                      #' Handles the insertion in mongo
                      to_mongo = function(mongo_con) {
                        ## TODO
                      }
                    )
)

SpadlOpta = R6::R6Class("SpadlOpta",
                        inherit = Spadl,
                        public = list(
                            initialize =
                                function(game_ids,
                                         fixture_con = .settings$fixtures_con,
                                         events_con = .settings$events_con,
                                         keypass_con =
                                             .settings[["playerKeyPasses_con"]],
                                         config = .settings$opta_config,
                                         spadl_cfg = .settings$spadl_config) {
                                    ## implement a method to fill in the data fields
                                    ## do it in another file
                                    self$data <- .opta_to_spadl(game_ids, events_con,
                                                                keypass_con, spadl_cfg,
                                                                config)
                                    ## call mother class initialize() to finish the job
                                    super$initialize(game_ids, fixture_con)
                                }
                        )
)

SpadlInStat = R6::R6Class("SpadlInStat",
                          inherit = Spadl,
                          public = list(
                              initialize = function(game_ids, fixture_con, events_con) {
                              ## implement a method to fill in the data fields
                              ## do it in another file
                              self$data <- .instat_to_spadl(game_ids, events_con)

                              ## call mother class initialize() to finish the job
                              super$initialize(game_ids, fixture_con)
                            }
                          )
)
