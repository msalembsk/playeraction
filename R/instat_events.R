#' @param spadl_cfg list giving the SPADL config. Default is to read it from
#'     global package config
#'
#' @return \code{tibble} representing instat to SPADL info.
#' @import dplyr tibble mongoTools
#' @export
.instat_events_from_game <- function(game_id,
                                     events_con = .settings[["gameEvents_con"]],
                                     instat_cfg = .settings$instat_config,
                                     spadl_cfg = .settings$spadl_config) {
  ## get events per game
  keys <- list(gameId = game_id)
  game_query <- buildQuery(names(keys), keys)
  game_info <- events_con$find(game_query)

  events <- game_info$events[[1]]

  ## check if retrieved events collection is empty
  if (nrow(events) == 0)
    return(tibble())

  ## home team id
  home_team_id <- game_info$homeTeamId

  ## fill missing bodypart with foot
  events$body_id[is.na(events$body_id)] <- 1L ## foot ID

  ## remove useless rows
  events <- events[!is.na(events$team_id), ]
  ## number of events row per game
  nrows <- nrow(events)
  ## join with bodypart
  events <- left_join(events, instat_cfg$bodypart_types,
                      by = c("body_id" = "body_id"))

  ## side
  side <- ifelse(events$team_id == home_team_id, "home", "away")
  events <- cbind(events, side = side)
  ## parse a single event by index
  .parse_single_event <- function(idx_row) {
    ## get event by id
    event_ <- events[idx_row, ]
    if (idx_row < nrows)
      next_event_ <- events[idx_row + 1, ]
    else
      next_event_ <- NULL

    c(second, minute, time_in_seconds) %<-%
      .time_in_seconds_minutes(event_$second, event_$half)

    spadl_action_name <- .get_spadl_action_name(event_, next_event_)



    tibble(game_id = game_id,
           event_id = event_$id,
           period_id = event_$half,
           second = second,
           minute = minute,
           time_in_seconds = time_in_seconds,
           start_x = event_$pos_x,
           start_y = event_$pos_y,
           end_x = event_$pos_dest_x,
           end_y = event_$pos_dest_y,
           player_id = event_$player_id,
           type_name = spadl_action_name,
           bodypart_name = event_$bodypart_name,
           bodypart_id = event_$bodypart_id,
           team_id = event_$team_id,
           side = event_$side,
           home_team_id = home_team_id,
           outcome = event_$outcome,
           action_id = event_$action_id
           )
  }
  ## get all events from a given game_id
  res <- do.call(rbind, lapply(seq_len(nrows), .parse_single_event)) %>%
    .result_type_name %>% filter(.data$type_name != "non_action") %>%
    .direction_play_pos(., spadl_cfg) %>% .clearance_pos %>%
    left_join(spadl_cfg$results,by = c("result_name" = "result_name")) %>%
    select(-c(outcome, action_id)) %>% left_join(
      spadl_cfg$actiontypes,by = c("type_name" = "action_name"))

  res
}


## second/ minute/ time in seconds function
.time_in_seconds_minutes <- function(time_in_seconds, half_period) {
  if (half_period == 1) {
    ## seconds
    seconds <- time_in_seconds %% 60
    ## minutes
    minutes <- as.integer(time_in_seconds / 60)
  } else {
    ## seconds
    seconds <- time_in_seconds %% 60
    ## minutes second half after 45 minutes
    minutes <- as.integer(time_in_seconds / 60) + 45

    ## time in seconds after second half adding 45 * 60
    time_in_seconds <-  time_in_seconds + 2700
  }


  c(seconds, minutes, time_in_seconds)
}

## function to call the specific fn action
## TODO : avoid if else statement and replace it with switch case
.get_spadl_action_name <- function(event_, next_event_,
                                   instat_cfg = .settings$instat_config) {
spadl_action_name <- "non_action"
 if (event_$generic_action_type_name %in% instat_cfg$shots)
   spadl_action_name <- .shot_action(event_)
 else if (event_$action_name %in% instat_cfg$bad_touch)
   spadl_action_name <- .bad_touch_action(event_)
 else if (event_$action_name %in% instat_cfg$dribble)
   spadl_action_name <- .dribble_action(event_)
 else if (event_$action_subtype_name %in% instat_cfg$take_on)
   spadl_action_name <- .take_on_action(event_)
 else if (event_$standart_name %in% instat_cfg$throw_in)
   spadl_action_name <- .throw_in_action(event_)
 else if (event_$action_name %in% instat_cfg$clearance)
   spadl_action_name <- .clearance_action(event_)
 else if (event_$action_name %in% instat_cfg$pick_up &
          event_$position_id %in% instat_cfg$gk_pos_id)
   spadl_action_name <- .keeper_pick_up_action(event_)
 else if (event_$action_name %in% instat_cfg$keeper_claim)
   spadl_action_name <- .keeper_claim_action(event_)
  else if (event_$action_name %in% instat_cfg$keeper_save)
   spadl_action_name <- .keeper_save_action(event_)
  else if (event_$action_subtype_name %in% instat_cfg$tackle)
    spadl_action_name <- .tackle_action(event_)
  else if (event_$generic_action_type_name %in% instat_cfg$interception)
    spadl_action_name <- .interception_action(event_)
  else if (event_$action_name %in% instat_cfg$foul)
    spadl_action_name <- .foul_action(event_)
  else if (event_$generic_action_type_name %in% instat_cfg$cross)
    spadl_action_name <- .cross_action(event_)
  else if (event_$standart_name %in% instat_cfg$freekick)
    spadl_action_name <- .freekick_action(event_, next_event_)
  else if (event_$standart_name %in% instat_cfg$corner)
    spadl_action_name <- .corner_action(event_, next_event_)
  else if (event_$standart_name %in% instat_cfg$pass_standart &
           event_$generic_action_type_name %in% instat_cfg$pass)
    spadl_action_name <- .pass_action(event_)

 spadl_action_name
}

## pass
.pass_action <- function(event_, action_types =
                           .settings$instat_config$action_types) {
  action_standart_id <- event_$standart_id
  spadl_action_name <- action_types[
    which(action_types$standart_id == action_standart_id), ]$spadl_name
  spadl_action_name
}

## corner
.corner_action <- function(event_, next_event_, action_types =
                             .settings$instat_config$action_types) {
  action_standart_id <- next_event_$standart_id
  action_subtype_name <- next_event_$action_subtype_name
  additional_marks <- event_$generic_action_type_id == 28L
  if (action_subtype_name == "set piece") {
    spadl_action_name <- action_types[
      which(action_types$standart_id == action_standart_id), ]$spadl_name
    return(spadl_action_name)
  }
  else if (!additional_marks & action_subtype_name != "set piece")
    return("corner_short")
  else
    return("non_action")

}

## freekick
.freekick_action <- function(event_, next_event_, action_types =
                                .settings$instat_config$action_types) {

  action_subtype_name <- next_event_$action_subtype_name
  additional_marks <- event_$generic_action_type_id == 28L

    if (action_subtype_name == "set piece") {
      spadl_action_name <- action_types[
        which(action_types$action_name == "free_kick"), ]$spadl_name
      return(spadl_action_name)
    }
    else if (!additional_marks & action_subtype_name != "set piece")
      return("freekick_short")
    else
      return("non_action")

}

## cross
.cross_action <- function(event_, action_types =
                            .settings$instat_config$action_types) {
  action_id <- event_$generic_action_type_id
    spadl_action_name <- action_types[
      which(action_types$generic_action_type_id == action_id), ]$spadl_name
  spadl_action_name
}

.foul_action <- function(event_, action_types =
                                  .settings$instat_config$action_types) {
  action_id <- event_$action_id
  spadl_action_name <- action_types[
    which(action_types$action_id == action_id), ]$spadl_name
  spadl_action_name
}

## cross
.interception_action <- function(event_, action_types =
                                  .settings$instat_config$action_types) {
  action_id <- event_$generic_action_type_id
  spadl_action_name <- action_types[
    which(action_types$generic_action_type_id == action_id), ]$spadl_name
  spadl_action_name
}


## tackle
.tackle_action <- function(event_, action_types =
                             .settings$instat_config$action_types) {
  action_name <- event_$action_subtype_name
  spadl_action_name <- action_types[
    which(action_types$action_subtype_name == action_name), ]$spadl_name
  spadl_action_name
}


## GK save
.keeper_save_action <- function(event_, action_types =
                                   .settings$instat_config$action_types) {
  action_id <- event_$action_id
  spadl_action_name <- action_types[
    which(action_types$action_id == action_id), ]$spadl_name
  spadl_action_name
}

## GK claim
.keeper_claim_action <- function(event_, action_types =
                                  .settings$instat_config$action_types) {
  action_id <- event_$action_id
  spadl_action_name <- action_types[
    which(action_types$action_id == action_id), ]$spadl_name
  spadl_action_name
}

## GK pickup
.keeper_pick_up_action <- function(event_, action_types =
                                     .settings$instat_config$action_types) {

action_id <- event_$action_id
spadl_action_name <- action_types[
  which(action_types$action_id == action_id), ]$spadl_name
spadl_action_name
}


## throw in
.throw_in_action <- function(event_, action_types =
                               .settings$instat_config$action_types) {
  standart_id <- event_$standart_id

  spadl_action_name <- action_types[
    which(action_types$standart_id == standart_id), ]$spadl_name
  spadl_action_name
}

## take on
.take_on_action <- function(event_, action_types =
                              .settings$instat_config$action_types) {
  action_name <- event_$action_subtype_name

  spadl_action_name <- action_types[
    which(action_types$action_subtype_name == action_name), ]$spadl_name
  spadl_action_name
}

## clearance
.clearance_action <- function(event_, action_types =
                              .settings$instat_config$action_types) {
  action_id <- event_$action_id

  spadl_action_name <- action_types[
    which(action_types$action_id == action_id), ]$spadl_name
  spadl_action_name
}

## dribble
.dribble_action <- function(event_, action_types =
                              .settings$instat_config$action_types) {
  action_id <- event_$action_id

  spadl_action_name <- action_types[
    which(action_types$action_id == action_id), ]$spadl_name
  spadl_action_name
}

## bad ball control
.bad_touch_action <- function(event_, action_types =
                                .settings$instat_config$action_types) {
  action_id <- event_$action_id

  spadl_action_name <- action_types[
    which(action_types$action_id == action_id), ]$spadl_name
  spadl_action_name
}

## shot/ penalty shot/ freekick shot
.shot_action <- function(event_,
                         shot_types = .settings$instat_config$shot_types) {

  action_standart_id <- event_$standart_id
  action_attack_type_id <- event_$attack_type_id
  if (is.element(action_attack_type_id, shot_types$attack_type_id)) {
    spadl_action <- shot_types[
      which(shot_types$attack_type_id == action_attack_type_id), ]$spadl_name
    return(spadl_action)
  } else if (is.element(action_standart_id, shot_types$standart_id)) {
    spadl_action <- shot_types[
      which(shot_types$standart_id == action_standart_id), ]$spadl_name
    return(spadl_action)
  } else
    return("shot")

}

## result spadl name
.result_type_name <- function(events) {
  events$result_name <- "fail"
  actions_ <- events[nrow(events), ]
  next_actions_ <- events[-1, ]
  second_next_actions_ <- events[-2, ]

  same_player <- actions_$player_id == next_actions_$player_id

  ## Yellow Card action ID (next event)
  is_yellow_card <- next_actions_$action_id == 3020L
  yellow_card_idx <- which(same_player & is_yellow_card)

  ## Red Card action ID (next event)
  is_red_card <- next_actions_$action_id == 3030L
  red_card_idx <- which(same_player & is_red_card)

  ## offside can be after opening of a pass
  ## not sure about the next action
  is_offide <- second_next_actions_$action_id == 3040L &
    second_next_actions_$second - actions_$second < 10

  offside_idx <- which(is_offide)

  goal_idx <- which(events$action_id == 8010L)
  owngoal_idx <- which(events$action_id == 8020L)

  events[offside_idx - 1, ]$result_name <- "offside"

  events[yellow_card_idx, ]$result_name <- "yellow_card"

  events[red_card_idx, ]$result_name <- "red_card"

  events[goal_idx, ]$result_name <- "success"

  events[owngoal_idx, ]$result_name <- "owngoal"

  is_success <-  !(actions_$type_name %in% c("shot", "foul", "offside")) &
    actions_$outcome

  success_idx <- which(is_success)

  events[success_idx, ]$result_name <- "success"

  events
}

## adjust playing direction
.direction_play_pos <- function(events, spadl_cfg = .settings$spadl_config) {
  is_away <- which(events$side == "away")
  events$start_x[is_away] <- spadl_cfg$field_length - events$start_x[is_away]
  events$end_x[is_away] <- spadl_cfg$field_length - events$end_x[is_away]
  events$start_y[is_away] <- spadl_cfg$field_width - events$start_y[is_away]
  events$end_y[is_away] <- spadl_cfg$field_width - events$end_y[is_away]
  events
}

## clearance shifted end position
.clearance_pos <- function(events) {
  next_actions_ <- events[-1, ]
  is_clearance <- which(events$type_name == "clearance")

  events$end_x[is_clearance] <- next_actions_$start_x[is_clearance]
  events$end_y[is_clearance] <- next_actions_$start_y[is_clearance]

  events
}
