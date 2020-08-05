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



  action_types <- instat_cfg$action_types

  ## get only useful events
  events <- events[which(events$generic_action_type_name %in% action_types), ]
  ## number of events row per game
  nrows <- nrow(events)
  ## fill missing bodypart with foot
  events$body_id[is.na(events$body_id)] <- 1L ## foot ID
  ## parse a single event by index
  .parse_single_event <- function(idx_row) {
    ## get event by id
    event_ <- events[idx_row, ]
    spadl_action_name <- .get_action_family(event_)


    period_id <- event_$half
    time_in_seconds <- event_$second

    player_id <- event_$player_id
    player_name <- event_$player_name

    team_id <- event_$team_id
    team_name <- event_$team_name


    tibble(game_id = game_id,
           home_team_id = home_team_id,
           home_team_name = home_team_name,
           away_team_id = away_team_id,
           away_team_name = away_team_name,
           player_id = player_id,
           player_name = player_name,
           team_id = team_id,
           team_name = team_name,
           period_id = period_id,
           time_in_seconds = time_in_seconds,
           action_name = spadl_action_name)


  }

  ## get all events from a given game_id
  res <- do.call(rbind, lapply(seq_len(nrows), .parse_single_event))

  res
}


## generic function to get the appropriate action_name
## TODO : try to replace if else statement to switch case
.get_action_family <- function(event_,
                               instat_cfg = .settings$instat_config) {
  action_type <- event_$generic_action_type_name
  action_subtype_name <- event_$action_subtype_name
  player_position <- event_$position_name

  action_name <- "non_action"
  if (action_type == "Pass"  & action_subtype_name %in%
      instat_cfg$action_pass)
    action_name <- .pass_action_family(event_)
  else if (action_type == "Shot"  & action_subtype_name %in%
           instat_cfg$action_shot)
    action_name <- .shot_action_family(event_)
  else if (action_type == "Clearance" & action_subtype_name %in%
           instat_cfg$action_clearance)
    action_name <- .clearance_action_family(event_)
  else if (action_type == "Discipline"  & action_subtype_name %in%
           instat_cfg$action_discipline)
    action_name <- .discipline_action_family(event_)
  else if (action_type == "Challenges" & action_subtype_name %in%
          instat_cfg$action_challenges)
    action_name <- .challenge_action_family(event_)
  else if (action_type == "Cross" & action_subtype_name %in%
           instat_cfg$action_cross)
    action_name <- .cross_action_family(event_)
  else if (action_type == "Interception" & action_subtype_name %in%
           instat_cfg$action_interception)
    action_name <- .interception_action_family(event_)
  else if (action_type == "Bad Ball Control" & action_subtype_name %in%
           instat_cfg$action_bad_control)
    action_name <- .bad_control_action_family(event_)
  else if (player_position == "Goalkeeper" & action_subtype_name %in%
           instat_cfg$action_gk)
    action_name <- .gk_action_family(event_)



  action_name
}


## pass action checker
.pass_action_family <- function(pass_event_,
                                 instat_cfg = .settings$instat_config) {
  action_name <- NULL
  ## check standart action for the right real action
  standart_action <- pass_event_$standart_name

  throw_in <- standart_action %in% instat_cfg$throw_in
  ## FIXME : check freekick short or crossed
  freekick <- standart_action %in% instat_cfg$freekick
  ## short corner else check cross action + corner attack function
  ## not sure
  corner <- standart_action %in% instat_cfg$corner
  action_name <-
    dplyr::case_when(throw_in ~ "throw_in",
                     freekick ~ "freekick",
                     corner ~ "corner",
                     TRUE ~ "pass")
  action_name
}

## interception action
.interception_action_family <- function(interception_event_,
                                        instat_cfg = .settings$instat_config) {
  interception_action  <- interception_event_$action_subtype_name %in%
    instat_cfg$action_interception
  if (interception_action)
    return("interception")
}


## bad ball control
.bad_control_action_family <- function(bad_control_event_,
                                       instat_cfg = .settings$instat_config) {
  ball_touch_action <- bad_control_event_$action_subtype_name %in%
    instat_cfg$action_bad_control

  if (ball_touch_action)
    return("ball_touch")
}

## cross action
.cross_action_family <- function(cross_event_,
                                 instat_cfg = .settings$instat_config) {

  cross_action <- cross_event_$action_subtype_name %in% instat_cfg$cross
  if (cross_action)
    return("cross")
}


## shot action checker
.shot_action_family <- function(shot_event_,
                                instat_cfg = .settings$instat_config) {
  action_name <- NULL
  ## check standart action for freekick shot
  shot_freekick <- shot_event_$standart_name %in% instat_cfg$shot_freekick

  if (shot_freekick)
    action_name <- "shot_freekick"
  else
    action_name <- "shot"

  action_name
}

## discipline action
.discipline_action_family <- function(discipline_event_,
                                       instat_cfg = .settings$instat_config) {
  action_name <- NULL
  foul_action <- discipline_event_$action_subtype_name %in% instat_cfg$foul
  if (foul_action)
    action_name <- "foul"

  action_name
}

## challenges actions
.challenge_action_family <- function(challenge_event_,
                                     instat_cfg = .settings$instat_config) {
  action_name <- NULL
  action_type <- challenge_event_$action_subtype_name

  take_on <- action_type %in% instat_cfg$take_on
  dribble <- action_type %in% instat_cfg$dribble
  tackle <- action_type %in% instat_cfg$tackle
  action_name <-
    dplyr::case_when(take_on ~ "take_on",
                     dribble ~ "dribble",
                     tackle ~ "tackle",
                     TRUE ~ "non_action")
  action_name

}

## clearance action
.clearance_action_family <- function(clearance_event_,
                                      instat_cfg = .settings$instat_config) {
  action_name <- NULL
  clearance_action <- clearance_event_$action_subtype_name %in%
    instat_cfg$action_clearance

  if (clearance_action)
    action_name <- "clearance"

  action_name
}

.gk_action_family <- function(gk_event_,
                              instat_cfg = .settings$instat_config) {
  action_name <- NULL

  action_type <- gk_event_$action_subtype_name

  gk_interception <- action_type %in% instat_cfg$gk_interception
  keeper_save <- action_type %in% instat_cfg$keeper_save
  keeper_pick_up <- action_type %in% instat_cfg$keper_pick_up

  if (keeper_save)
    action_name <- "keeper_save"
  else if (gk_interception)
    action_name <- "interception"
  else if (keeper_pick_up)
    action_name <- "keeper_pick_up"

  action_name
}
