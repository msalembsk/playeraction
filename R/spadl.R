#' @family Spadl
#' @import R6 data.table
Spadl = R6::R6Class("Spadl",
                    public = list(
                        #' @field data `data.table` :: the SPADL values stored as data.table
                        data = NULL,

                        #' @field metadata `data.table` :: metadata such as team name,
                        #' game-date, ... should have the same number of rows
                        #' as `data` and same game-ids.
                        metadata = NULL,

                        #' Creates the 2 `data.table`(s) from mongo connections.
                        #' Given that the fixture collection is similar for all
                        #' data providers, the `metadata` field can be initialised here.
                        #' Must be implemented in each subclass (for each data provider).
                        #' @param game_ids `integer`
                        #' @param fixtures_con `mongo-connection` :: fixture collection
                        #' connection
                        initialize = function(game_ids, fixture_con) {
                            ## TODO:
                            ## 1) metata initialisation here
                            ## 2) data left as NULL
                        },

                        #' Runs some sanity checks on the created instance
                        #' Regardless of the data provider (and the subclass used
                        #' to initialise the object), we expect the fields
                        #' values to be the same and hence sanity checks can be
                        #' implemented here.
                        #' @return logical :: `TRUE` if all
                        #' tests are passed otherwise `FALSE` with an informative
                        #' error message
                        sanity_check = function() {
                            ## TODO
                        },

                        #' Insert the instance in the mongo database if sanity-checks are successful
                        #' @param mongo_con `mongo-connection` :: where the object should be inserted
                        #' @return logical :: insertion status
                        insert = function(mongo_con) {
                            ## TODO
                        },

                        #' Converts the object to a  `data.table`
                        #'
                        #' We simply join the `metadata` and `data` fields by `gameId`.
                        #' @return `data.table`
                        as_table = function() {
                            left_join(self$metdata, self$data, by = "gameId")
                        }
                    ),
                    private = list(
                        #' Handles the insertion in mongo
                        #' @param mongo_con `mongo-connection` :: where the object should be inserted
                        to_mongo = function(mongo_con) {
                            ## TODO
                        }
                    )
                    )

#' @family Spadl
#' @export
SpadlOpta = R6::R6Class("SpadlOpta",
                        inherit = "Spadl",
                        public = list(
                            #' Creates Spadl object from opta data
                            #' @param events_con `mongo-connection` :: connection to the
                            #' opta Events collection
                            initialize = function(game_ids, fixture_con, events_con) {
                                ## TODO
                                ## implement a method to fill in the data fields
                                ## do it in another file
                                self$data <- .opta_to_spadl(game_ids, events_con)

                                ## call mother class initialize() to finish the job 
                                super$initialize(game_ids, fixture_con)
                            }
                        )
                        )

#' @family Spadl
#' @export
SpadlInStat = R6::R6Class("SpadlInStat",
                          inherit = "Spadl",
                          public = list(
                              #' Creates Spadl object from inStat data
                              #' @param events_con `mongo-connection` :: connection to the
                              #' inStat Events collection
                              initialize = function(game_ids, fixture_con, events_con) {
                                  ## implement a method to fill in the data fields
                                  ## do it in another file
                                  self$data <- .instat_to_spadl(game_ids, events_con)

                                  ## call mother class initialize() to finish the job 
                                  super$initialize(game_ids, fixture_con)
                              }
                          )
                          )
