library(mongoTools)
library(dplyr)
library(futile.logger)
library(zeallot)
library(tibble)
library(lubridate)
library(magrittr)

## Leicester vs Man utd
## game_id <- 1171071L

devtools::load_all()
playeraction::set_db(database_type = "local", data_provider = "inStat")

game_ids <- .settings$gameEvents_con$distinct("gameId")
spadl_instat <- SpadlInStat$new(game_ids, spadl_type = "atomic")
flog.info(paste("Spadl representation created !"))
spadl_instat$get_model_data(add_predictions = TRUE)
flog.info(paste("training data created !"))

