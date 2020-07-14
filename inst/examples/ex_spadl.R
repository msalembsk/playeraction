library(mongoTools)
library(dplyr)
library(futile.logger)
library(zeallot)
library(tibble)
library(lubridate)
library(magrittr)


playeraction::set_db_all(database_type = "local", data_provider = "opta")

game_id <- 44108
events <- playeraction::.opta_events_from_game(game_id)
spdal_events <- playeraction::convert_events_to_spadl.opta_events(events)

featured_events <- playeraction::.spadl_to_features(spdal_events)

