library(mongoTools)

spadl_type <- "atomic"
keys <- list(seasonId = 2019, competitionId = c(8, 22))
qr <- buildQuery(names(keys), keys)

game_ids <- .settings$gameEvents_con$distinct("gameId", qr)

spadl_opta <- SpadlOpta$new(game_ids, spadl_type = "atomic")
flog.info(paste("Spadl representation created !"))

spadl_opta$get_model_data(add_predictions = FALSE)
flog.info(paste("training data created !"))
