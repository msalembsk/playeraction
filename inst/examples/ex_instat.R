library(mongoTools)
library(dplyr)
library(futile.logger)
library(zeallot)
library(tibble)
library(lubridate)
library(magrittr)

## Leicester vs Man utd
game_id <- 1267383L
devtools::load_all()
playeraction::set_db(database_type = "local", data_provider = "inStat")
inStat_spadl <- playeraction::.instat_events_from_game(game_id)

## game_id <- 44108
## events <- playeraction::.opta_events_from_game(game_id)
## spadl_events <- playeraction::convert_events_to_spadl.opta_events(events)

## featured_events <- playeraction::.spadl_to_features(spadl_events)

## labels_events <- playeraction::.spadl_to_labels(spadl_events)
