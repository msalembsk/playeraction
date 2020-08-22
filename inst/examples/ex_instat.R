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
inStat_spadl <- playeraction::.instat_events_from_game(game_id)


