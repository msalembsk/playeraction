#' @param events list events after formating
#' @param config  list  spadl attributs
#' @export
.events_to_spadl <- function(events, config = .settings$spadl_config) {


  spdal_cfg <- .load_config()

  ## number of events row per game
  nrows <- nrow(events)


  events <- events[with(events, order(events$min, events$sec)), ]

  .parse_event <- function(idx_row) {
    event_ <- events[idx_row, ]

    ## time in seconds
    time_in_seconds_ <- 60 * event_$min + event_$sec


    ## start coordinates formatting
    x_pos_coord <- sapply(c(event_$start_x, event_$end_x),
           function(x) x / 100 * spdal_cfg$field_length)
    event_$start_x <- x_pos_coord[1]
    event_$end_x <- x_pos_coord[2]

    ## end coordinates formatting
    y_pos_coord <- sapply(c(event_$start_y, event_$end_y),
           function(y) y <- y / 100 * spdal_cfg$field_width)
    event_$start_y <- y_pos_coord[1]
    event_$end_y <- y_pos_coord[2]

    ## body part index
    body_part_id_ <- .get_body_parts(
      event_$qualifiers.qualifiers[[1]])

    ## add new columns to the event
    cbind(event_, body_part_id = body_part_id_,
                    time_in_seconds = time_in_seconds_)

  }

  events <- do.call(rbind, lapply(1:nrows, .parse_event))
  merge(x = events, y = spdal_cfg$event_types, by.x = "type_id",
        by.y = "event_id", all.x = TRUE)

}

## get body part index
.get_body_parts <- function(qualifiers) {

  qualifiers_keys <- names(qualifiers)
  if ("15" %in% qualifiers_keys)
    return(2L)
  else if ("21" %in% qualifiers_keys)
    return(3L)
  else
    return(1L)
}


## load spadl config and enumrate types
.load_config <- function(config = .settings$spadl_config) {

  ## action type
  action_types <- data.frame(type_id = seq_along(config$actiontypes),
                             type_name = config$actiontypes)

  ## event types
  event_types <- data.frame(event_id = seq_along(config$eventtypes),
                            event_name = config$eventtypes)
  ## body parts
  bodyparts <- data.frame(bodypart_id = seq_along(config$bodyparts),
                          bodypart_name = config$bodyparts)

  ## result type
  results <- data.frame(result_id = seq_along(config$results),
                        result_name = config$results)
  field_length <- config$field_length
  field_width <- config$field_width
  min_dribble_length <- config$min_dribble_length
  max_dribble_length <- config$max_dribble_length
  max_dribble_duration <- config$max_dribble_duration

  list(action_types = action_types,
             event_types = event_types,
             bodyparts = bodyparts,
             results = results,
             field_length = field_length,
             field_width = field_width,
             min_dribble_length = min_dribble_length,
             max_dribble_duration = max_dribble_duration,
             max_dribble_length = max_dribble_length)
}
