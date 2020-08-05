#' convert SPADL events to features
#'
#' @param events events from SPADL
#' @param fixtures_con fixtures db connection
#' @return \code{tibble} representing features details.
#' @export
.spadl_to_features <- function(events, fixtures_con = .settings$fixtures_con) {
    ## if spadl_events empty
    if (nrow(events) == 0)
        return(tibble())

    ## get game_id from a single event
    game_id <- events[1, ]$game_id
    ## get game info
    keys <- list(gameId = game_id)
    fixtures_query <- buildQuery(names(keys), keys)
    game_info <- fixtures_con$find(fixtures_query)

    ## home team id
    home_team_id <- game_info$homeTeamId

    ## fix start end coordinates
    events <- events %>% .fix_start_end_coor(home_team_id = home_team_id)

    ## simple features
    type_id_features <- events$type_id %>% .shift_event_values %>%
        .bind_columns_features(attr = "type_id") %>% as.data.frame()

    body_part_id_features <- events$type_id %>% .shift_event_values %>%
        .bind_columns_features(attr = "body_part_id") %>% as.data.frame()

    result_id_features <- events$type_id %>% .shift_event_values %>%
        .bind_columns_features(attr = "result_id") %>% as.data.frame()

    start_end_features <- events %>% .start_end_x_y() %>% as.data.frame()

    type_name_features <- events %>% .type_name_features(type = "type") %>%
        as.data.frame()

    body_part_name_features <- events %>%
        .type_name_features(type = "body_part") %>% as.data.frame()

    result_name_features <- events %>%
        .type_name_features(type = "result") %>% as.data.frame()

    ## start end polar call
    start_polar <- events %>%  .start_polar_features()
    end_polar <- events %>% .end_polar_features()

    start_dist_to_goal <- start_polar$start_dist_to_goal %>%
        .shift_event_values %>%
        .bind_columns_features(attr = "start_dist_to_goal") %>%
        as.data.frame()

    start_angle_to_goal <- start_polar$start_angle_to_goal %>%
        .shift_event_values %>%
        .bind_columns_features(attr = "start_angle_to_goal") %>%
        as.data.frame()

    end_dist_to_goal <- end_polar$end_dist_to_goal %>%
        .shift_event_values %>%
        .bind_columns_features(attr = "end_dist_to_goal") %>%
        as.data.frame()

    end_angle_to_goal <- end_polar$end_angle_to_goal %>%
        .shift_event_values %>%
        .bind_columns_features(attr = "end_angle_to_goal") %>%
        as.data.frame()

  ## movement position call
    movement_features <-  events %>% .movement_features()

    dx <- movement_features$dx %>% .shift_event_values %>%
        .bind_columns_features(attr = "dx") %>% as.data.frame()

    dy <- movement_features$dy %>% .shift_event_values %>%
        .bind_columns_features(attr = "dy") %>% as.data.frame()

    movement <- movement_features$movement %>% .shift_event_values %>%
        .bind_columns_features(attr = "movement") %>% as.data.frame()

    ## team features
    team_features <- events %>% .team_features()

    ## delta time features
    delta_times <-  events %>% .delta_time_features()

    ## delta space features
    delta_space <- events %>% .space_delta()

    ## goal score teams features
    goal_score <- events %>% .goal_score_features()

    ## time features
    time_feature <- events %>% .time_features()

    period_id <- time_feature$period_id %>% .shift_event_values %>%
        .bind_columns_features(attr = "period_id") %>% as.data.frame()

    time_seconds <- time_feature$time_seconds %>% .shift_event_values %>%
        .bind_columns_features(attr = "time_seconds") %>% as.data.frame()

    time_seconds_overall <- time_feature$time_seconds_overall %>%
        .shift_event_values %>%
        .bind_columns_features(attr = "time_seconds_overall") %>%
        as.data.frame()

    tibble(type_id_features, body_part_id_features,
           result_id_features, start_end_features,
           type_name_features, body_part_name_features,
           result_name_features, start_dist_to_goal,
           start_angle_to_goal, end_dist_to_goal,
           end_angle_to_goal, team_features,
           dx, dy,
           movement, delta_times,
           delta_space, goal_score,
           period_id, time_seconds,
           time_seconds_overall)
}


.start_end_x_y <- function(events) {
    start_x_features <- events$start_x %>% .shift_event_values %>%
        .bind_columns_features(attr = "start_x") %>% as.data.frame()

    start_y_features <- events$start_y %>% .shift_event_values %>%
        .bind_columns_features(attr = "start_y") %>% as.data.frame()

    end_x_features <- events$end_x %>% .shift_event_values %>%
        .bind_columns_features(attr = "end_x") %>% as.data.frame()

    end_y_features <- events$end_y %>% .shift_event_values %>%
        .bind_columns_features(attr = "end_y") %>% as.data.frame()

    tibble(start_x_features,
           start_y_features,
           end_x_features,
           end_y_features)
}

.shift_event_values <- function(values) {
    ## shift events by 2
    previous_values <- values %>% data.table::shift(1L, type = "lag")
    second_previous_values <- values %>% data.table::shift(2L, type = "lag")

    list(current_values = values,
         previous_values = previous_values,
         second_previous_values = second_previous_values)
}

##FIX ME : need to be optimized
## fix start end x y
.fix_start_end_coor <- function(events, home_team_id, spadl_cfg =
                                  .settings$spadl_config) {

    ## x coords
  events$start_x[events$team_id != home_team_id] <-
      spadl_cfg$field_length - events$start_x[events$team_id != home_team_id]
    events$end_x[events$team_id != home_team_id] <-
        spadl_cfg$field_length - events$end_x[events$team_id != home_team_id]

    ## y coords
    events$start_y[events$team_id != home_team_id] <-
        spadl_cfg$field_width - events$start_y[events$team_id != home_team_id]
    events$end_y[events$team_id != home_team_id] <-
        spadl_cfg$field_width - events$end_y[events$team_id != home_team_id]

    events
}


## type name features (type action , result , body part)
.type_name_features <- function(events, type = c("type", "result", "body_part"),
                                spadl_cfg = .settings$spadl_config) {
    if (type == "type")
        type_names <- spadl_cfg$actiontypes$action_name
    else if (type == "result")
        type_names <- spadl_cfg$results$result_name
    else
        type_names <- spadl_cfg$bodyparts$bodypart_name

    event_type_name <- paste0(type, "_name")
    events_type <- events[[event_type_name]] %>% .shift_event_values

    .fetch_types <- function(type_name) {
        ## build the name action
        col_name <- paste0(type, "_", type_name)
        values <- lapply(events_type, function(x) x == type_name)
        .bind_columns_features(col_name, values)
    }

    lapply(type_names, .fetch_types)
}

## start polar position
.start_polar_features <- function(events, spadl_cfg = .settings$spadl_config) {
    goal_x <- spadl_cfg$goal_x
    goal_y <- spadl_cfg$goal_y

    ## x y distance
    distance_x <- sapply(events$start_x, function(x) abs(goal_x - x))
    distance_y <- sapply(events$start_y, function(x) abs(goal_y - x))

    ## start euclidian distance
    start_dist_to_goal  <- sqrt(distance_x^2 + distance_y^2)
    ## start angle to goal
    start_angle_to_goal <- atan(distance_y / distance_x)

    list(start_dist_to_goal = start_dist_to_goal,
         start_angle_to_goal = start_angle_to_goal)
}

## end polar position
.end_polar_features <- function(events, spadl_cfg = .settings$spadl_config) {
  goal_x <- spadl_cfg$goal_x
  goal_y <- spadl_cfg$goal_y

  ## x y distance
  distance_x <- sapply(events$end_x, function(x) abs(goal_x - x))
  distance_y <- sapply(events$end_y, function(x) abs(goal_y - x))


  ## end euclidian distance
  end_dist_to_goal  <- sqrt(distance_x^2 + distance_y^2)
  ## end angle to goal
  end_angle_to_goal <- atan(distance_y / distance_x)


  list(end_dist_to_goal = end_dist_to_goal,
       end_angle_to_goal = end_angle_to_goal)
}

## movement features
.movement_features <- function(events) {
  ## distance x y
  distance_x <- events$end_x - events$start_x
  distance_y <- events$end_y - events$start_y
  ## end euclidian distance
  movement <- sqrt(distance_x^2 + distance_y^2)

  list(dx = distance_x,
       dy = distance_y,
       movement = movement)
}

.team_features <- function(events) {
  teams <- events$team_id %>% .shift_event_values()

  ## check if the same team still playing
  team_1 <- teams$current_values == teams$previous_values
  team_2 <- teams$current_values == teams$second_previous_values


  tibble(team_1 = team_1, team_2 = team_2)
}

.delta_time_features <- function(events) {
  time_in_seconds <- events$minute * 60 + events$second

  times <- time_in_seconds %>% .shift_event_values()
  ## check if the same team still playing
  time_delta_1 <- times$current_values -
    times$previous_values
  time_delta_2 <- times$current_values -
    times$second_previous_values


  tibble(time_delta_1 = time_delta_1, time_delta_2 = time_delta_2)
}


.space_delta <- function(events) {

  start_x <-  events$start_x %>% .shift_event_values()
  start_y <-  events$start_y %>% .shift_event_values()

  end_x <-  events$end_x %>% .shift_event_values()
  end_y <-  events$end_y %>% .shift_event_values()

  ## previous events distance
  previous_dx <- end_x$previous_values - start_x$current_values
  previous_dy <- end_y$previous_values - start_y$current_values

  dx_a01 <- previous_dx
  dy_a01 <- previous_dy

  mov_a01 <- sqrt(dx_a01^2 + dy_a01^2)

  ## second previous events distance
  s_previous_dx <- end_x$second_previous_values - start_x$current_values
  s_previous_dy <- end_y$second_previous_values - start_y$current_values

  dx_a02 <- s_previous_dx
  dy_a02 <- s_previous_dy
  mov_a02 <- sqrt(dx_a02^2 + dy_a02^2)

  tibble(dx_a01 = dx_a01,
         dy_a01 = dy_a01,
         mov_a01 = mov_a01,
         dx_a02 = dx_a02,
         dy_a02 = dy_a02,
         mov_a02 = mov_a02)
}

.goal_score_features <- function(events) {

  team_a <- events[1]$team_id
  goals <- grepl("shot", events$type_name) & events$result_name == "success"
  owngoals <- grepl("shot", events$type_name) & events$result_name == "owngoal"
  team_is_a <- events$team_id == team_a
  team_is_b <- !team_is_a
  goals_team_a <- (goals & team_is_a) | (owngoals & team_is_b)
  goals_team_b <- (goals & team_is_b) | (owngoals & team_is_a)
  goalscore_team_a <- cumsum(goals_team_a) - goals_team_a
  goalscore_team_b <- cumsum(goals_team_b) - goals_team_b

  goalscore_team <-  goalscore_team_a * team_is_a +
    goalscore_team_b * team_is_b

  goalscore_opponent <- goalscore_team_b * team_is_a +
    goalscore_team_a * team_is_b

  goalscore_diff <- goalscore_team - goalscore_opponent

  tibble(goalscore_team = goalscore_team,
         goalscore_opponent = goalscore_opponent,
         goalscore_diff = goalscore_diff)
}

## time overall features
.time_features <- function(events) {
  period_id <- events$period_id
  time_seconds <- events$minute * 60 + events$second
  time_seconds_overall <- (period_id - 1) * 45 * 60 +
    time_seconds

  list(period_id = period_id,
         time_seconds = time_seconds,
       time_seconds_overall = time_seconds_overall)
}

## generic features function to bind 2 previous event with the current one
.bind_columns_features <- function(attr, values, nb_lag = 3) {
  out_ <- list()
  for (idx in seq_len(nb_lag)) {
    ## column name by id
    column_name <- paste0(attr, "_a", as.character(idx))
    out_[[column_name]] <- values[[idx]]
  }
  out_
}
