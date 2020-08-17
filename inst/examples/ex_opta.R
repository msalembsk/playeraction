library(mongoTools)
library(futile.logger)

game_id <- 1060077

spadl_opta <- SpadlOpta$new(game_id, spadl_type = "atomic")
flog.info(paste("Spadl representation created !"))

spadl_opta$get_model_data(add_predictions = FALSE)
flog.info(paste("training data created !"))
