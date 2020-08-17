#' Translating raw data to SPADL representation
#'
#' We use SPADL common language (see details in the paper) to represent raw data
#' regardless of the data provider used. Every data provider has his own
#' implement class inheriting from the mother class.
#' @name Spadl
#' @import R6 tibble data.table
Spadl = R6::R6Class("Spadl",
                    public = list(
                        spadl_type = "standard",
                        #' @field data \code{data.table} :: the SPADL values stored as data.table
                        data = NULL,

                        #' @field metadata \code{tibble} :: metadata such as team name,
                        #' game-date, ... should have the same number of rows
                        #' as `data` and same game-ids.
                        metadata = NULL,

                        #' @field training_data
                        training_data = NULL,

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
                      #' Create model training data from Spadl or atomic Spadl
                      #'
                      #' @return populates the \code{training_data} slot.
                      get_model_data = function(nb_prev_actions = 3L, nr_actions = 10L,
                                                labels = TRUE,
                                                add_predictions = TRUE,
                                                scores_learner = .settings$model_scores,
                                                concedes_learner =
                                                    .settings$model_concedes) {
                          spadl_dt <- self$data

                          ## find all game ids
                          all_game_ids <- unique(spadl_dt[["game_id"]])
                          .wh <- function(gm_id) {
                              ans <- try(
                                  .get_model_data(as_tibble(spadl_dt[game_id == gm_id]),
                                                  nb_prev_actions, nr_actions, labels,
                                                  self$spadl_type)
                              )
                              if (!inherits(ans, "try-error"))
                                  return(ans)
                          }

                          ## training data
                          dt <- pblapply(all_game_ids, .wh) %>% rbindlist()
                          dt <- dt[, lapply(.SD, as.numeric)]
                          dt[["scores"]] <- factor(ifelse(dt[["scores"]],
                                                          "goal", "no_goal"),
                                                   c("goal", "no_goal")
                                                   )
                          dt[["concedes"]] <- factor(ifelse(dt[["concedes"]],
                                                          "goal", "no_goal"),
                                                     c("goal", "no_goal")
                                                     )

                          ## task scores
                          if (add_predictions) {
                              if (length(all_game_ids) > 1)
                                  stop("We should not predict more than one game at a time !")
                              task_score <-
                                  mlr3::TaskClassif$new(id = "scores",
                                                        backend = select(dt, -.data$concedes),
                                                        target = "scores")
                              task_concede <-
                                  mlr3:: TaskClassif$new(id = "concedes",
                                                         backend = select(dt, -.data$scores),
                                                         target = "concedes")

                              ## probability
                              scores_pb <- scores_learner$predict(task_score,
                                                                  row_ids = 1:nrow(dt)
                                                                  ) %>%
                                  as.data.table() %>%
                                  as.tibble()
                              concedes_pb <- concedes_learner$predict(task_concede,
                                                                      row_ids = 1:nrow(dt)
                                                                      ) %>%
                                  as.data.table() %>%
                                  as.tibble()

                              dt <- dplyr::mutate(dt,
                                                  scores = scores_pb[["prob.goal"]],
                                                  concedes = concedes_pb[["prob.goal"]])
                              spadl_dt <- dplyr::mutate(spadl_dt,
                                                        scores = scores_pb[["prob.goal"]],
                                                        concedes = concedes_pb[["prob.goal"]]
                                                        ) %>%
                                  .get_vaep_values()
                          }
                          ## update object
                          self$training_data <- dt
                          self$data <- spadl_dt
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
                                         events_con = .settings$gameEvents_con,
                                         keypass_con =
                                             .settings[["playerKeyPasses_con"]],
                                         config = .settings$opta_config,
                                         spadl_cfg = .settings$spadl_config,
                                         spadl_type = c("standard", "atomic")) {
                                    self$spadl_type <- match.arg(spadl_type)
                                    ## call mother class initialize() to finish the job
                                    super$initialize(game_ids, fixture_con)

                                    ## implement a method to fill in the data fields
                                    ## do it in another file
                                    self$data <- .opta_to_spadl(game_ids, events_con,
                                                                keypass_con, spadl_cfg,
                                                                config, self$spadl_type)
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
