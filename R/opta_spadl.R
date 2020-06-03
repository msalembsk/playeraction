#' convert opta-events to SPADL
#'
#' convert opta-events to SPADL
#'
#' @param object an object from class \code{opta_events}
#' @param ... ex
#' @return \code{tibble} representing SPADL info.
#' @export
#' @rdname spadl_conversion
convert_events_to_spadl.opta_events <- function(
                                                object,
                                                spadl_cfg =
                                                    .settings$spadl_config,
                                                opta_cfg =
                                                    .settings$opta_config,
                                                ...) {
  ## read events from opta_events class
  events <- attr(object, "opta_events")
  ## number of events row per game
  nrows <- nrow(events)
  ## FIXME: use dplyr arrange and include period_id in the sort
  events <- events[with(events, order(events$min, events$sec)), ]

  .parse_event <- function(idx_row) {
    event_ <- events[idx_row, ]

    ## time in seconds
    time_in_seconds_ <- 60 * event_$min + event_$sec

    ## start coordinates formatting
    x_pos_coord <- sapply(
      c(event_$start_x, event_$end_x),
      function(x) x / 100 * spadl_cfg$field_length
    )
    event_$start_x <- x_pos_coord[1]
    event_$end_x <- x_pos_coord[2]

    ## end coordinates formatting
    y_pos_coord <- sapply(
      c(event_$start_y, event_$end_y),
      function(y) y <- y / 100 * spadl_cfg$field_width
    )
    event_$start_y <- y_pos_coord[1]
    event_$end_y <- y_pos_coord[2]

    ## body part index
    body_part_id_ <- .get_body_parts(
      spadl_cfg$bodyparts$bodypart_name,
      event_$qualifiers.qualifiers[[1]],
      opta_cfg[["Q_head"]],
      opta_cfg[["Q_other"]]
    )

    ## left join for event name
    event_ <- event_ %>% left_join(opta_cfg$type_table,
      by = c("type_id" = "typeId")
    )

    ## action type name
    action_type_name <- .get_action_type(event_)

    ## result type name
    result_type_name <- .get_result_type(
      event_,
      opta_config[["owngoal"]]
    )

    ## add new columns to the event
    event_ <- cbind(event_,
      body_part_id = body_part_id_,
      time_in_seconds = time_in_seconds_,
      action_type_name = action_type_name,
      result_type_name = result_type_name
    )

    event_ %>% .owngoal_x_y()
  }

  do.call(rbind, lapply(1:nrows, .parse_event))
}

## get body part index
.get_body_parts <- function(bodypart_name, qualifiers, q_head, q_other) {
  qualifiers_keys <- names(qualifiers)
  if (q_head %in% qualifiers_keys) {
    return(which(bodypart_name == "head"))
  } else if (q_other %in% qualifiers_keys) {
    return(which(bodypart_name == "other"))
  } else {
    return(which(bodypart_name == "foot"))
  }
}

## action types
.get_action_type <- function(event,
                             opta_config = .settings$opta_config) {

  action_name <- NA
  ## to character event_name comes as a factor
  event_name <- as.character(event$event_name)

  ## qualifiers
  qualifiers_keys <- names(event$qualifiers.qualifiers[[1]])

  ## load different action types
  action_types <- opta_config$action_types
  action_shots <- opta_config$action_shots
  action_pass <- opta_config$action_pass
  action_foul <- opta_config$action_foul
  action_touch <- opta_config$action_touch

  ## standard action
  if (event_name %in% action_types) {
    action_name <- opta_config[event_name][[1]]
  }
  ## action pass
  else if (event_name %in% action_pass) {
    ## FIXED: hard coding move to opta config
    freekick <- opta_config[["Q_freekick"]] %in% qualifiers_keys

    cross <- opta_config[["Q_cross"]] %in% qualifiers_keys

    corner <- opta_config[["Q_corner"]] %in% qualifiers_keys

    throw_in <- opta_config[["Q_throw_in"]] %in% qualifiers_keys

    if (throw_in) {
      action_name <- opta_config["throw_in"][[1]]
    } else if (freekick & cross) {
      action_name <- opta_config["freekick_crossed"][[1]]
    } else if (freekick) {
      action_name <- opta_config["freekick_short"][[1]]
    } else if (corner & cross) {
      action_name <- opta_config["corner_crossed"][[1]]
    } else if (corner) {
      action_name <- opta_config["corner_short"][[1]]
    } else if (cross) {
      action_name <- opta_config["cross"][[1]]
    } else {
      action_name <- opta_config["pass"][[1]]
    }
  }
  ## action shot
  ## FIXED: hard coding move to opta config
  else if (event_name %in% action_shots) {
    if (opta_config[["shot_penalty"]] %in% qualifiers_keys) {
      action_name <- opta_config["shot_penalty"][[1]]
    } else if (opta_config[["shot_freekick"]] %in% qualifiers_keys) {
      action_name <- opta_config["shot_freekick"][[1]]
    } else {
      action_name <- opta_config["shot"][[1]]
    }
  }
  ## action touch
  else if (event_name %in% action_touch & !event$outcome) {
    action_name <- opta_config["bad_touch"][[1]]
  } ## action foul
  else if (event_name %in% action_foul & !event$outcome) {
    action_name <- opta_config["foul"][[1]]
  } else {
    action_name <- "non_action"
  }

  action_name
}

## coordinates owngoal
.owngoal_x_y <- function(event, spadl_cfg = .settings$spadl_config) {
  ## recalculate x & y if result type is an owngoal
  if (event$result_type_name == "owngoal") {
    ## end x & y new values
    event$end_y <- spadl_cfg$field_width - event$end_y
    event$end_x <- spadl_cfg$field_length - event$end_x
  }
  event
}

## results types
.get_result_type <- function(event, q_owngoal) {
  event_name <- as.character(event$event_name)
  qualifiers_keys <- names(event$qualifiers.qualifiers[[1]])
  if (event_name == "offside pass") {
    result_name <- "offside"
  } else if (event_name == "goal") {
    if (q_owngoal %in% qualifiers_keys) {
      result_name <- "owngoal"
    } else {
      result_name <- "success"
    }
  }
  else if (event$outcome) {
    result_name <- "success"
  } else {
    result_name <- "fail"
  }

  result_name
}
