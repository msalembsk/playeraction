library(mongoTools)
library(purrr)

## mongo connections
spadl_con <- mongo("Spadl", "opta")
spadl_features_con <- mongo("SpadlFeatures", "opta")
spadl_atomic_con <- mongo("AtomicSpadl", "opta")
spadl_atomic_features_con <- mongo("AtomicSpadlFeatures", "opta")


keys <- list(seasonId = 2018:2019, competitionId = c(8, 10, 21:24))
qr <- buildQuery(names(keys), keys)

game_ids <- .settings$gameEvents_con$distinct("gameId", qr)
in_game_ids <- spadl_atomic_features_con$distinct("game_id")
mss_game_ids <- game_ids[!game_ids %in% in_game_ids]

.wh <- function(game_id) {
    ## standard spadl
    spadl_opta_std <- SpadlOpta$new(game_id, spadl_type = "standard")
    spadl_opta_std$get_model_data(add_predictions = FALSE)
    spadl_opta_std_dt <- spadl_opta_std$data
    spadl_opta_std_training_dt <- spadl_opta_std$training_data

    spadl_opta_std_dt[["_id"]] <- paste0(spadl_opta_std_dt$event_id, "_",
                                         spadl_opta_std_dt$player_id, "_",
                                         spadl_opta_std_dt$type_name)
    spadl_opta_std_training_dt[["_id"]] <-
        paste0(spadl_opta_std_training_dt$event_id, "_",
               spadl_opta_std_training_dt$type_id_a0)
    spadl_opta_std_training_dt[["game_id"]] <- game_id

    ## standard atomic
    spadl_opta_atomic <- SpadlOpta$new(game_id, spadl_type = "atomic")
    spadl_opta_atomic$get_model_data(add_predictions = FALSE)
    spadl_opta_atomic_dt <- spadl_opta_atomic$data
    spadl_opta_atomic_training_dt <- spadl_opta_atomic$training_data

    spadl_opta_atomic_dt[["_id"]] <- paste0(spadl_opta_atomic_dt$event_id, "_",
                                            spadl_opta_atomic_dt$player_id, "_",
                                            spadl_opta_atomic_dt$type_name, "_",
                                            spadl_opta_atomic_dt$action_id
                                            )

    spadl_opta_atomic_training_dt[["_id"]] <-
        paste0(spadl_opta_atomic_training_dt$event_id, "_",
               spadl_opta_atomic_training_dt$type_id_a0, "_",
               spadl_opta_atomic_training_dt$type_id_a1, "_",
               spadl_opta_atomic_training_dt$type_id_a2
               )
    spadl_opta_atomic_training_dt[["game_id"]] <- game_id

    ## insert in db
    spadl_con$insert(spadl_opta_std_dt)
    spadl_features_con$insert(spadl_opta_std_training_dt)
    spadl_atomic_con$insert(spadl_opta_atomic_dt)
    spadl_atomic_features_con$insert(spadl_opta_atomic_training_dt)
}

x <- map(mss_game_ids, safely(.wh))
