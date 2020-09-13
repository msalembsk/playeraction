library(mongoTools)
library(purrr)

## mongo connections
spadl_con <- mongo("Spadl", "inStat")
spadl_features_con <- mongo("SpadlFeatures", "inStat")
spadl_atomic_con <- mongo("AtomicSpadl", "inStat")
spadl_atomic_features_con <- mongo("AtomicSpadlFeatures", "inStat")
playeraction::set_db(database_type = "local", data_provider = "inStat")

SEASON_IDS <- 2016:2019
COMP_IDS <- c(316, ## Tunisia Ligue 1
              193, ## Algeria Ligue1
              140, ## Egypt Premier League
              112, ## Morocco GNF 1
              118, ## Qatar Stars League
              296, ## Saudi Arabia Pro League
              167, ## CAF Champions League
              170 ## Africa. CAF Confederation Cup
              )
keys <- list(seasonId = SEASON_IDS, competitionId = COMP_IDS)
qr <- buildQuery(names(keys), keys)

game_ids <- .settings$gameEvents_con$distinct("gameId", qr)
in_game_ids <- spadl_atomic_features_con$distinct("game_id")
mss_game_ids <- game_ids[!game_ids %in% in_game_ids]

.wh <- function(game_id) {
    ## standard spadl
    spadl_inStat_std <- SpadlInStat$new(game_id, spadl_type = "standard")
    spadl_inStat_std$get_model_data(add_predictions = FALSE)
    spadl_inStat_std_dt <- spadl_inStat_std$data
    spadl_inStat_std_training_dt <- spadl_inStat_std$training_data

    spadl_inStat_std_dt[["_id"]] <- paste0(spadl_inStat_std_dt$event_id, "_",
                                           game_id, "_",
                                           spadl_inStat_std_dt$player_id, "_",
                                           spadl_inStat_std_dt$type_name)
    spadl_inStat_std_training_dt[["_id"]] <-
        paste0(spadl_inStat_std_training_dt$event_id, "_", game_id, "_",
               spadl_inStat_std_training_dt$type_id_a0)
    spadl_inStat_std_training_dt[["game_id"]] <- game_id

    ## standard atomic
    spadl_inStat_atomic <- SpadlInStat$new(game_id, spadl_type = "atomic")
    spadl_inStat_atomic$get_model_data(add_predictions = FALSE)
    spadl_inStat_atomic_dt <- spadl_inStat_atomic$data
    spadl_inStat_atomic_training_dt <- spadl_inStat_atomic$training_data

    spadl_inStat_atomic_dt[["_id"]] <- paste0(spadl_inStat_atomic_dt$event_id, "_",
                                              game_id, "_",
                                              spadl_inStat_atomic_dt$player_id, "_",
                                              spadl_inStat_atomic_dt$type_name, "_",
                                              spadl_inStat_atomic_dt$action_id
                                              )

    spadl_inStat_atomic_training_dt[["_id"]] <-
        paste0(spadl_inStat_atomic_training_dt$event_id, "_",
               game_id, "_",
               spadl_inStat_atomic_training_dt$type_id_a0, "_",
               spadl_inStat_atomic_training_dt$type_id_a1, "_",
               spadl_inStat_atomic_training_dt$type_id_a2
               )
    spadl_inStat_atomic_training_dt[["game_id"]] <- game_id

    ## insert in db
    spadl_con$insert(spadl_inStat_std_dt)
    spadl_features_con$insert(spadl_inStat_std_training_dt)
    spadl_atomic_con$insert(spadl_inStat_atomic_dt)
    spadl_atomic_features_con$insert(spadl_inStat_atomic_training_dt)
}

##x <- map(mss_game_ids, safely(.wh))
