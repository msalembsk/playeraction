library(mongoTools)
library(dplyr)
library(futile.logger)
library(zeallot)
library(tibble)
library(lubridate)
library(magrittr)


playeraction::set_db_all(database_type = "read", data_provider = "opta")

game_id <- 805408
events <- .extract_events_from_game(game_id)
