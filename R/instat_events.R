#' @import tibble mongoTools
#' @export
.instat_events_from_game <- function(game_id,
                                   events_con = .settings$GameEvents_con,
                                   instat_cfg = .settings$instat_config) {

  ## get events per game
  keys <- list(gameId = game_id)
  game_query <- buildQuery(names(keys), keys)
  game_info <- events_con$find(game_query)

  events <- game_info$events[[1]]

  ## check if retrieved events collection is empty
  if (nrow(events) == 0)
    return(tibble())

  ## generic game info
  home_team_id <- game_info$homeTeamId
  home_team_name <- game_info$homeTeamName

  away_team_id <- game_info$awayTeamId
  away_team_name <- game_info$awayTeamName

  ## number of events row per game
  nrows <- nrow(events)
  ## fill missing bodypart with foot
  events$body_id[is.na(events$body_id)] <- 1L ## foot ID
  ## parse a single event by index
  .parse_single_event <- function(idx_row) {
    ## get event by id
    event_ <- events[idx_row, ]

    period_id <- event_$half

    player_id <- event_$player_id
    player_name <- event_$player_name

    team_id <- event_$team_id
    team_name <- event_$team_name

    c(second, minute, time_in_seconds) %<-%
      .time_in_seconds_minutes(event_$second, event_$half)

    spadl_action_name <- .get_spadl_action_name(event_)

    tibble(game_id = game_id,
           home_team_id = home_team_id,
           home_team_name = home_team_name,
           away_team_id = away_team_id,
           away_team_name = away_team_name,
           period_id = period_id,
           second = second,
           minute = minute,
           time_in_seconds = time_in_seconds,
           player_id = player_id,
           player_name = player_name,
           team_id = team_id,
           team_name = team_name,
           action_name = spadl_action_name)


  }
  ## get all events from a given game_id
  res <- do.call(rbind, lapply(seq_len(nrows), .parse_single_event)) %>%
    filter(.data$action_name != "non_action")

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
.get_spadl_action_name <- function(event_,
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
    spadl_action_name <- .freekick_action(event_)
  else if (event_$standart_name %in% instat_cfg$corner)
    spadl_action_name <- .corner_action(event_)
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
.corner_action <- function(event_, action_types =
                             .settings$instat_config$action_types) {
  action_standart_id <- event_$standart_id
  action_subtype_name <- event_$action_subtype_name
  if (action_subtype_name == "set piece") {
    spadl_action_name <- action_types[
      which(action_types$standart_id == action_standart_id), ]$spadl_name
    return(spadl_action_name)
  }
  else
    return("corner_short")
}

## freekick
.freekick_action <- function(event_, action_types =
                                .settings$instat_config$action_types) {
  action_standart_id <- event_$standart_id
  action_subtype_name <- event_$action_subtype_name
  direct_freekick_id <- 4L
  indirect_freekick_id <- 3L
  if (action_standart_id == direct_freekick_id) {
    if (action_subtype_name == "set piece") {
      spadl_action_name <- action_types[
        which(action_types$standart_id == direct_freekick_id), ]$spadl_name
      return(spadl_action_name)
    }
    else
      return("freekick_short")
  } else if (action_standart_id == indirect_freekick_id) {
    if (action_subtype_name == "set piece") {
      spadl_action_name <- action_types[
        which(action_types$standart_id == indirect_freekick_id), ]$spadl_name
      return(spadl_action_name)
    }
    else
      return("freekick_short")
  }
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





