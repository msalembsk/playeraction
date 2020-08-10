#' @include opta_events.R
NULL

#' @importFrom data.table setDT rbindlist data.table
#' @importFrom pbapply pblapply
.opta_to_spadl <- function(game_ids,
                           events_con = .settings$events_con,
                           keypass_con = .settings[["playerKeyPasses_con"]],
                           spadl_cfg = .settings$spadl_config,
                           opta_cfg = .settings$opta_config) {
  ## work horse
  .wh <- function(game_id) {
    .opta_events_from_game(game_id,
                           events_con = events_con,
                           keypass_con = keypass_con,
                           opta_cfg = opta_cfg) %>%
      convert_events_to_spadl(spadl_cfg = spadl_cfg,
                              opta_cfg = opta_cfg)
  }

  if (length(game_ids) == 1)
    purrr::map_dfr(game_ids, .wh)
  else
    pblapply(game_ids, .wh) %>% rbindlist()
}

#' convert opta-events to SPADL
#'
#' @param object an object from class \code{opta_events}
#' @param spadl_cfg list giving the SPADL config. Default is to read it from
#'     global package config
#' @param opta_cfg list giving the opta config. Default is to read it from
#'     global package config
#' @param ... extra parameters currently not used
#' @return \code{tibble} representing SPADL info.
#' @importFrom dplyr filter select pull
#' @export
#' @rdname spadl_conversion
convert_events_to_spadl.opta_events <- function(events,
                                                spadl_cfg =
                                                  .settings$spadl_config,
                                                opta_cfg =
                                                  .settings$opta_config,
                                                ...) {
  ## arrange event in chronological order
  events <- dplyr::arrange(events,
                           .data$period_id, .data$minute, .data$second) %>%
    filter(.data$period_id %in% c(1, 2)) %>%
    left_join(opta_cfg$type_table, by = c("type_id" = "typeId"))

  ## number of events row per game
  nrows <- nrow(events)

  .parse_event <- function(idx_row) {
    event_ <- events[idx_row, ]

    ## time in seconds
    time_in_seconds_ <- 60 * event_$minute + event_$second

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
      event_$qualifiers[[1]],
      opta_cfg[["Q_head"]],
      opta_cfg[["Q_other"]]
    )

    ## body part name
    body_part_name_ <- spadl_cfg$bodyparts$bodypart_name[body_part_id_]

    ## action type name
    action_type_name <- .get_action_type(event_)

    ## result type name
    result_type_name <- .get_result_type(
      event_,
      opta_cfg[["owngoal"]]
    )

    idx_result_id <- which(
      spadl_cfg$results$result_name == result_type_name)
    result_id_ <- spadl_cfg$results$result_id[idx_result_id]

    ## remove old type_id
    event_ <- event_ %>% select(-type_id)

    ## add new columns to the event
    event_ <- cbind(event_,
                    body_part_name = body_part_name_,
                    body_part_id = body_part_id_,
                    time_in_seconds = time_in_seconds_,
                    type_name = action_type_name,
                    result_id = result_id_,
                    result_name = result_type_name
    ) %>%
      .owngoal_x_y() %>%
      .adjust_direction_play(spadl_cfg)

    if (idx_row != nrows)
      event_ <- .check_clearance(event_,
                                 .parse_event(idx_row + 1)
      )

    ## action type id
    type_id <- filter(spadl_cfg$actiontypes,
                      .data$action_name == event_$type_name) %>%
      pull(.data$action_id)
    ## bind type_id after checking clearance and dribble
    cbind(event_, type_id = type_id)
  }

  do.call(rbind, lapply(seq_len(nrows), .parse_event)) %>%
    filter(.data$type_name != "non_action") %>%
    .add_dribbles(spadl_cfg = spadl_cfg) %>%
    select(-c(.data$qualifiers, .data$outcome, .data$type))
}

.add_dribbles <- function(events, spadl_cfg = .settings$spadl_config) {
  actions_ <- events[-nrow(events), ]
  next_actions_ <- events[-1, ]

  same_team <- actions_$team_id == next_actions_$team_id

  dx <- actions_$end_x - next_actions_$start_x
  dy <- actions_$end_y - next_actions_$start_y
  far_enough <- (dx ** 2 + dy ** 2) >= spadl_cfg$min_dribble_length ** 2
  not_too_far <- (dx ** 2 + dy ** 2) <= spadl_cfg$max_dribble_length ** 2

  dt <- next_actions_$time_in_seconds - actions_$time_in_seconds
  same_phase <- dt < spadl_cfg$max_dribble_duration

  dribble_idx <- which(same_team & far_enough & not_too_far & same_phase)

  prev <- actions_[dribble_idx, ]
  nex <- next_actions_[dribble_idx, ]
  dribbles <- nex
  dribbles$time_in_seconds <- 0.5 * (prev$time_in_seconds +
                                       nex$time_in_seconds)
  dribbles$start_x <- prev$end_x
  dribbles$start_y <- prev$end_y
  dribbles$end_x <- nex$start_x
  dribbles$end_y <- nex$start_y
  dribbles$body_part_name <- "foot"
  dribbles$body_part_id <- filter(spadl_cfg$bodyparts,
                                  .data$bodypart_name == "foot") %>%
    pull(.data$bodypart_id)
  dribbles$type_name <- "dribble"
  dribbles$type_id <- filter(spadl_cfg$actiontypes,
                             .data$action_name == "dribble") %>%
    pull(.data$action_id)
  dribbles$result_name <- "success"
  dribbles$result_id <- filter(spadl_cfg$results,
                               .data$result_name == "success") %>%
    pull(.data$result_id)

  rbind(events, na.omit(dribbles)) %>%
    arrange(.data$period_id, .data$time_in_seconds)
}

.adjust_direction_play <- function(event_, spadl_cfg) {
  if (event_$side == "away") {
    event_$start_x <- spadl_cfg$field_length - event_$start_x
    event_$end_x <- spadl_cfg$field_length - event_$end_x

    event_$start_y <- spadl_cfg$field_width - event_$start_y
    event_$end_y <- spadl_cfg$field_width - event_$end_y
  }

  event_
}

.check_clearance <- function(event_, next_event_,
                             opta_cfg = .settings$opta_config) {
  if (event_$type_name == opta_cfg[["clearance"]][[1]]) {
    event_$end_x <- next_event_$start_x
    event_$end_y <- next_event_$start_y
  }
  event_
}

## get body part index
.get_body_parts <- function(bodypart_name, qualifiers, q_head, q_other) {
  qualifiers_keys <- names(qualifiers)
  if (any(q_head %in% qualifiers_keys))
    which(bodypart_name == "head")
  else if (q_other %in% qualifiers_keys)
    which(bodypart_name == "other")
  else
    which(bodypart_name == "foot")
}

## action types
.get_action_type <- function(event,
                             opta_cfg = .settings$opta_config) {
  action_name <- NA
  ## to character event_name comes as a factor
  event_name <- as.character(event$type)

  ## qualifiers
  qualifiers_keys <- names(event$qualifiers[[1]])

  ## load different action types
  action_types <- opta_cfg$action_types
  action_shots <- opta_cfg$action_shots
  action_pass <- opta_cfg$action_pass
  action_foul <- opta_cfg$action_foul
  action_touch <- opta_cfg$action_touch

  ## standard action
  if (event_name %in% action_types)
    action_name <- opta_cfg[event_name][[1]]
  else if (event_name %in% action_pass) {
    freekick <- opta_cfg[["Q_freekick"]] %in% qualifiers_keys
    cross <- opta_cfg[["Q_cross"]] %in% qualifiers_keys
    corner <- opta_cfg[["Q_corner"]] %in% qualifiers_keys
    throw_in <- opta_cfg[["Q_throw_in"]] %in% qualifiers_keys

    action_name <-
      dplyr::case_when(
        throw_in ~ opta_cfg["throw_in"][[1]],
        corner & cross ~ opta_cfg["corner_crossed"][[1]],
        freekick & cross ~ opta_cfg["freekick_crossed"][[1]],
        cross ~ opta_cfg["cross"][[1]],
        freekick ~ opta_cfg["freekick_short"][[1]],
        corner ~ opta_cfg["corner_short"][[1]],
        TRUE ~ opta_cfg["pass"][[1]]
      )
  } else if (event_name %in% action_shots) {
    action_name <-
      dplyr::case_when(
        opta_cfg[["Q_shot_penalty"]] %in%
          qualifiers_keys ~ opta_cfg["shot_penalty"][[1]],
        opta_cfg[["Q_shot_freekick"]] %in%
          qualifiers_keys ~ opta_cfg["shot_freekick"][[1]],
        TRUE ~ opta_cfg["shot"][[1]]
      )
  } else if (event_name %in% action_touch & !event$outcome)
    ## action touch
    action_name <- opta_cfg["bad_touch"][[1]]
  else if (event_name %in% action_foul & !event$outcome)
    ## action foul
    action_name <- opta_cfg["foul"][[1]]
  else
    action_name <- "non_action"

  action_name
}

## coordinates owngoal
.owngoal_x_y <- function(event, spadl_cfg = .settings$spadl_config) {
  ## recalculate x & y if result type is an owngoal
  if (event$result_name == "owngoal") {
    ## end x & y new values
    event$end_y <- spadl_cfg$field_width - event$end_y
    event$end_x <- spadl_cfg$field_length - event$end_x
  }
  event
}

## results types
.get_result_type <- function(event, q_owngoal) {
  event_name <- as.character(event$type)
  qualifiers_keys <- names(event$qualifiers[[1]])
  if (event_name == "offside pass")
    result_name <- "offside"
  else if (event_name == "foul")
    result_name <- "fail"
  else if (event_name %in% c("attempt saved", "miss", "post"))
    result_name <- "fail"
  else if (event_name == "goal") {
    if (!length(qualifiers_keys) && q_owngoal %in% qualifiers_keys)
      result_name <- "owngoal"
    else
      result_name <- "success"
  } else if (event_name == "ball touch")
    result_name <- "fail"
  else if (event$outcome)
    result_name <- "success"
  else
    result_name <- "fail"

  result_name
}
