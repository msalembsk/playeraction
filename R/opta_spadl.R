.event_to_spadl <- function(events, config = .settings$spadl_config) {
  action_types <- data.frame(type_id = seq_along(config$actiontypes),type_name = config$actiontypes)
  event_types <- config$eventtypes
  bodyparts <- config$bodyparts
  results <- config$results
  field_length <- config$field_length
  field_width <- config$field_width
}


.action_types_values <- function
