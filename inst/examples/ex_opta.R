library(mongoTools)
library(futile.logger)

## Leicester vs Man utd
game_id <- 1060077
## Serie B
game_id <- 1016937
playeraction::set_db(database_type = "local", data_provider = "opta")
spadl_opta <- SpadlOpta$new(game_id, spadl_type = "standard")
flog.info(paste("Spadl representation created !"))

spadl_opta$get_model_data(add_predictions = TRUE)
flog.info(paste("training data created !"))

pl_index <- group_by(spadl_opta$data, player_id) %>%
    summarise(index = sum(vaep_value)) %>%
    arrange(desc(index))

keys <- list(playerId = pl_index$player_id)
qr <- buildQuery(names(keys), keys)
out <- list(playerId = 1, name = 1, position = 1, "_id" = 0)
qo <- buildQuery(names(out), out)
pl_info <- .settings$players_con$find(qr, qo)
pl_index <- left_join(pl_index,
                      rename(pl_info, player_id = .data$playerId),
                      by = "player_id")
