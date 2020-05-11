library(mongoTools)
library(dplyr)
library(futile.logger)
library(zeallot)
library(tibble)
library(lubridate)
library(magrittr)


playeraction::set_db_all(database_type = "local", data_provider = "opta")

game_id <- 44108
events <- .extract_events_from_game(game_id)
