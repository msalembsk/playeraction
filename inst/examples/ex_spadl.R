library(mongoTools)
library(dplyr)
library(futile.logger)
library(zeallot)
library(tibble)
library(lubridate)
library(magrittr)

devtools::load_all()
playeraction::set_db_all(database_type = "local", data_provider = "inStat")
spadl_inStat_events <- playeraction::.instat_events_from_game(1485159)

game_id <- 1060077
opta_events <- playeraction::.opta_events_from_game(game_id)
spadl_events <- playeraction::convert_events_to_spadl.opta_events(opta_events)

featured_events <- playeraction::.spadl_to_features(spadl_events)

labels_events <- playeraction::.spadl_to_labels(spadl_events)




