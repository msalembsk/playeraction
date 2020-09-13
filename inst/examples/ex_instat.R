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
COMP_IDS <- c(316, ## Tunisia Ligue 1
  193, ## Algeria Ligue1
  140, ## Egypt Premier League
  112, ## Morocco GNF 1
  118, ## Qatar Stars League
  296, ## Saudi Arabia Pro League
  167, ## CAF Champions League
  170 ## Africa. CAF Confederation Cup
)
keys <- list( competitionId = COMP_IDS)
qr <- buildQuery(names(keys), keys)

game_ids <- .settings$gameEvents_con$distinct("gameId", qr)
spadl_instat <- SpadlInStat$new(game_ids, spadl_type = "standard")
flog.info(paste("Spadl representation created !"))

spadl_instat$get_model_data(add_predictions = FALSE)
flog.info(paste("training data created !"))
