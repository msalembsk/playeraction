library(mongoTools)
library(dplyr)
library(futile.logger)
library(zeallot)
library(tibble)
library(lubridate)
library(magrittr)

## Leicester vs Man utd
game_id <- 1485159L
devtools::load_all()
playeraction::set_db(database_type = "local", data_provider = "inStat")
spadl_instat <- SpadlInStat$new(game_id, spadl_type = "standard")
flog.info(paste("Spadl representation created !"))



