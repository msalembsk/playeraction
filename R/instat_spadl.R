#' @export
.instat_to_spadl <- function(game_ids, events_con = .settings$gameEvents_con,
                             instat_cfg = .settings$instat_config,
                             spadl_cfg = .settings$spadl_config,
                             spadl_type = c("standard", "atomic")) {
  spadl_type <- match.arg(spadl_type)
  ## work horse
  .wh <- function(game_id) {
     out <- .convert_instat_events_spadl(game_id, events_con = events_con,
                  instat_cfg = instat_cfg,
                  spadl_cfg = spadl_cfg)
    ## extract some useful info
    home_team_ <- out$home_team_id[1]
    if (spadl_type == "atomic") {
      out <- .convert_spadl_to_atomic(mutate(out,
                                             action_id = seq_len(nrow(out))
      )
      ) %>%
        mutate(home_team_id = home_team_) %>%
        left_join(socceraction_py$atomic$spadl$actiontypes_df(),
                  by = "type_id") %>%
        left_join(spadl_cfg$bodyparts, by = "bodypart_id")
    }

    out

  }

  pblapply(game_ids, .wh) %>% rbindlist()

}



#' @param spadl_cfg list giving the SPADL config. Default is to read it from
#'     global package config
#'
#' @return \code{tibble} representing instat to SPADL info.
#' @import dplyr tibble mongoTools
#' @export
.convert_instat_events_spadl <- function(game_id,
                                     events_con = .settings$gameEvents_con,
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
  events$body_id[which(is.na(events$body_id))] <- 1L ## foot ID

  ## add time in seconds
  events <- events %>% cbind(time_in_seconds =  .get_time_in_seconds(events),
                             type_name = as.character("non_action"),
                             result_name = as.character("fail"),
                             side = NA) %>% .playing_side(., home_team_id)

  spadl_events <- .get_spadl_type_events(events)

  non_action_events <-  subset(events, !(events$id %in% spadl_events$id))

  events <- rbind(non_action_events, spadl_events)

  ## arrange event in chronological order
  events <- dplyr::arrange(events, .data$time_in_seconds)
  events <- events  %>%
    .result_type_name %>% filter(.data$type_name != "non_action") %>%
    .direction_play_pos(., spadl_cfg) %>% .clearance_pos %>%
    .instat_to_spadl_columns(., game_id = game_id %>% as.integer(),
                             home_team_id = home_team_id,
                             instat_cfg, spadl_cfg) %>% .fix_end_action_position

  events
}

.fix_end_action_position <- function(events) {
same_start_pos <- c("tackle", "interception", "bad_touch",
                         "take_on", "keeper_pick_up", "keeper_save")

next_start_pos <- c("dribble", "clearance")

is_same_start_pos_idx <- which(events$type_name %in% same_start_pos)
is_next_start_pos_idx <- which(events$type_name %in% next_start_pos)


events$end_x[is_same_start_pos_idx] <- events$start_x[is_same_start_pos_idx]
events$end_y[is_same_start_pos_idx] <- events$start_y[is_same_start_pos_idx]

events$end_x[is_next_start_pos_idx] <- events$start_x[is_next_start_pos_idx + 1]
events$end_y[is_next_start_pos_idx] <- events$start_y[is_next_start_pos_idx + 1]


events
}

.playing_side <- function(events, home_team_id) {
  is_team_id <- events$team_id == home_team_id
  events$side[which(is_team_id)] <- "home"
  events$side[which(!is_team_id)] <- "away"
  events
}

.get_time_in_seconds <- function(events) {
  time_in_seconds <- ifelse(events$half == 2,
                       events$second  + 2700,
                       events$second)

  time_in_seconds
}

.instat_to_spadl_columns <- function(events, game_id, home_team_id,
                                     instat_cfg = .settings$instat_config,
                                     spadl_cfg = .settings$spadl_config) {
  ## remove factor levels
  events$type_name <- as.character(events$type_name)
  ## seconds
  seconds <- as.integer(events$time_in_seconds %% 60)
  ## minutes
  minutes <- as.integer(events$time_in_seconds / 60)


  events <-  events %>% select(instat_cfg$spdal_instat_columns) %>%
    rename(event_id = id,
           start_x = pos_x,
           start_y = pos_y,
           end_x = pos_dest_x,
           end_y = pos_dest_y,
           period_id = half,
           time_seconds = time_in_seconds) %>%
    cbind(game_id = game_id,
          home_team_id = home_team_id,
          second = seconds,
          minute = minutes) %>%
    left_join(spadl_cfg$results, by = c("result_name" = "result_name"))  %>%
    left_join(spadl_cfg$actiontypes, by = c("type_name" = "action_name")) %>%
    rename(type_id = action_id) %>%
    left_join(instat_cfg$bodypart_types, by = c("body_id" = "body_id")) %>%
    select(-c(body_id))

  events
}

.get_spadl_type_events <- function(events) {

  shots_events <- .get_shots(events)
  freekick_events <- .get_freekick(events)
  corner_events <- .get_corners(events)

  tackles_events <- .get_tackles(events)
  take_on_events <- .get_take_on(events)
  dribbles_events <- .get_dribbles(events)

  throw_in_events <- .get_throw_in(events)

  interception_events <- .get_interceptions(events)

  pass_events <- .get_pass(events)

  keeper_save_events <- .get_keeper_save(events)
  keeper_pick_up_events <- .get_keeper_pick_up(events)

  clearance_events <- .get_clearance(events)
  bad_touch_events <- .get_bad_touch(events)


  rbind(shots_events,
        freekick_events,
        corner_events,
        tackles_events,
        throw_in_events,
        interception_events,
        pass_events,
        take_on_events,
        dribbles_events,
        keeper_save_events,
        keeper_pick_up_events,
        clearance_events,
        bad_touch_events)
}

.get_shots <- function(events) {
  ##  direct freekick ID == 4
  is_freekick <- events$standart_id == 4L
  ## corner ID == 5
  is_corner <- events$standart_id == 5L
  is_shot <- events$generic_action_type_id == 4L

  is_goal <- events$action_id == 8010L

  is_penalty <- events$standart_id == 6L

  shots_idx <- which(!is_freekick & !is_corner & !is_penalty &
                       (is_shot | is_goal))
  shot_events <- events[shots_idx, ]

  shot_events$type_name <- "shot"

  penalty_shots_idx <- which(is_penalty & (is_shot | is_goal))
  penalty_shots <- events[penalty_shots_idx, ]
  penalty_shots$type_name <- "shot_penalty"

  rbind(shot_events, penalty_shots)
}

.get_freekick <- function(events) {

  is_direct_freekick <- events$standart_id == 4L
  is_indirect_freekick <- events$standart_id == 3L
  ## first action in freekick
  is_start <- events$attack_status_id == 1L

  freekick_idx <- which((is_direct_freekick | is_indirect_freekick) & is_start)
  raw_freekick_events <- events[freekick_idx, ]


  prev_freekick_events <- events[freekick_idx - 1, ]

  ## remove ball out field and status match actions
  ## remove first freekick in the game refeer to first half action
  is_real_freekick_idx <- which(!prev_freekick_events[-1, ]$
                                  generic_action_type_id
                                %in% c(27L, 18L))

  freekick_events <- raw_freekick_events[is_real_freekick_idx, ]
  ## initialize type name as freekick_short
  freekick_events$type_name <- "freekick_short"


  ## check next actions to verify crossed freekick or not
  next_freekick_events <- events[as.numeric(rownames(freekick_events)) + 1, ]
  ## the set piece info can be after offside info
  second_next_freekick_events <- events[as.numeric(
    rownames(freekick_events)) + 2, ]

  ## crossed freekick
  is_crossed <- grepl("cross", next_freekick_events$action_name)
  is_second_crossed <- grepl("cross", second_next_freekick_events$action_name)

  ## crossed shot
  is_shot <- freekick_events$generic_action_type_id == 4L

  freekick_events$type_name[
    which(is_crossed | is_second_crossed)] <- "freekick_crossed"

  freekick_events$type_name[which(is_shot)] <- "shot_freekick"

  freekick_events
}

.get_dribbles <- function(events) {

  dribble_idx <- which(events$action_id == 21000L)

  dribble_events <- events[dribble_idx, ]
  dribble_events$type_name <- "dribble"

  dribble_events
}

.get_tackles <- function(events) {

## unsuccessfull_dribbling action ID : 2052
## successfull tackle action ID : 2031
tackle_idx <- which(events$action_id %in% c(2052L, 2031L))
tackles_events <- events[tackle_idx, ]
tackles_events$type_name <- "tackle"

tackles_events
}

.get_crosses <- function(events) {

  cross_idx <- which(events$generic_action_type_id == 26L)

  cross_events <- events[cross_idx, ]
  cross_events$type_name <- "cross"

  cross_events
}

.get_fouls <- function(events) {

  foul_idx <- which(events$action_id == 3010L)

  foul_events <- events[foul_idx, ]
  foul_events$type_name <- "foul"

  foul_events
}

.get_interceptions <- function(events) {

  interpections_idx <- which(events$action_id == 6020L)

  interception_events <- events[interpections_idx, ]
  interception_events$type_name <- "interception"

  interception_events
}

.get_corners <- function(events) {

  is_corner <- events$standart_id == 5L
  is_start <- events$attack_status_id == 1L

  corner_idx <- which(is_corner & is_start)

  corner_events <- events[corner_idx, ]

  corner_events$type_name <- "corner_short"

  ## check next actions to verify crossed corner or not
  next_corner_events <- events[as.numeric(rownames(corner_events)) + 1, ]

  ## crossed corner
  is_crossed <- grepl("cross", next_corner_events$action_name)
  corner_events$type_name[which(is_crossed)] <- "corner_crossed"

  corner_events
}

.get_pass <- function(events) {
  is_open_play <- events$standart_id == 1L
  is_pass <- events$generic_action_type_id == 1L

  pass_events <- events[which(is_open_play & is_pass), ]

  pass_events$type_name <- "pass"

  pass_events
}

.get_throw_in <- function(events) {
  throw_in_idx <- which(events$standart_id == 2L)

  throw_in_events <- events[throw_in_idx, ]
  throw_in_events$type_name <- "throw_in"

  throw_in_events
}

.get_take_on <- function(events) {

  take_on_idx <- which(events$action_id == 21000L)

  take_on_events <- events[take_on_idx, ]
  take_on_events$type_name <- "take_on"

  take_on_events
}

.get_keeper_save <- function(events) {

  ## consider good interception as a save action_ID :13011
  keeper_save_idx <- which(events$action_id %in% c(13040L, 13011L))
  keeper_save_events <- events[keeper_save_idx, ]
  keeper_save_events$type_name <- "keeper_save"

  keeper_save_events
}

.get_keeper_pick_up <- function(events) {
  is_pick_up <- events$action_id == 7000
  is_gk <- events$position_id == 31L

  keeper_pick_up_idx <- which(is_pick_up & is_gk)
  keeper_pick_up_events <- events[keeper_pick_up_idx, ]
  keeper_pick_up_events$type_name <- "keeper_pick_up"

  keeper_pick_up_events
}

.get_clearance <- function(events) {
  clearance_idx <- which(events$action_id == 9000L)

  clearance_events <- events[clearance_idx, ]
  clearance_events$type_name <- "clearance"

  clearance_events
}

.get_bad_touch <- function(events) {
  bad_touch_idx <- which(events$action_id == 10000L)

  bad_touch_events <- events[bad_touch_idx, ]
  bad_touch_events$type_name <- "bad_touch"

  bad_touch_events
}

## result spadl name
.result_type_name <- function(events) {
  events$result_name <- as.character(events$result_name)
  actions_ <- events[-nrow(events), ]
  next_actions_ <- events[-1, ]
  second_next_actions_ <- events[-2, ]

  same_player <- actions_$player_id == next_actions_$player_id

  .check_exist <- function(result) {
    if (length(result) == 0)
      return(FALSE)
    return(TRUE)
  }

  ## Yellow Card action ID (next event)
  is_yellow_card <- next_actions_$action_id == 3020L
  yellow_card_idx <- which(same_player & is_yellow_card)

  if (.check_exist(yellow_card_idx))
  actions_[yellow_card_idx, ]$result_name <- "yellow_card"

  ## Red Card action ID (next event)
  is_red_card <- next_actions_$action_id == 3030L
  red_card_idx <- which(same_player & is_red_card)

  if (.check_exist(red_card_idx))
  actions_[red_card_idx, ]$result_name <- "red_card"

  ## offside can be after opening of a pass
  ## not sure about the next action
  is_offside <- second_next_actions_$action_id == 3040L &
    second_next_actions_$second - actions_$second < 10
  offside_idx <- which(is_offside)

  if (.check_exist(offside_idx))
  actions_[offside_idx, ]$result_name <- "offside"

  goal_idx <- which(events$action_id == 8010L)

  if (.check_exist(goal_idx))
  actions_[goal_idx, ]$result_name <- "success"



  owngoal_idx <- which(events$action_id == 8020L)

  if (.check_exist(owngoal_idx))
  actions_[owngoal_idx, ]$result_name <- "owngoal"

  is_success <-  !(actions_$type_name %in% c("shot", "foul", "offside")) &
    actions_$outcome
  success_idx <- which(is_success)

  if (.check_exist(success_idx))
  actions_[success_idx, ]$result_name <- "success"

  actions_
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
