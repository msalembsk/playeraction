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

  home_team_id <- game_info$homeTeamId
  ## number of events row per game
  nrows <- nrow(events)

  .features_processing <- function(idx_event) {

    if (idx_event > 2) {
        current_event <- events[idx_event, ] %>% .fix_start_end_coor(
          home_team_id = home_team_id)
        previous_event <- events[idx_event - 1, ] %>% .fix_start_end_coor(
          home_team_id = home_team_id)
        second_previous_event <- events[idx_event - 2, ] %>%
          .fix_start_end_coor(home_team_id = home_team_id)

        ## bind 3 events to feature processing
        events <- rbind(current_event, previous_event, second_previous_event)

        ## action types
        type_id_feature <- events %>% .type_id_feature()
        type_names_feature <- events %>% .type_name_feature(type = "type")

        ## result types
        result_id_feature <- events %>% .result_id_feature()
        result_names_feature <- events %>% .type_name_feature(type = "result")

        ## body part types
        body_part_id_feature <- events %>% .body_part_id_feature()
        body_part_names_feature <- events %>%
          .type_name_feature(type = "body_part")

        ## start x y
        start_x_y_feature <- events %>% .start_x_y_feature()

        ## end x y
        end_x_y_feature <- events %>% .end_x_y_feature()

        cbind(type_id_feature,
              type_names_feature,
              result_id_feature,
              result_names_feature,
              body_part_id_feature,
              body_part_names_feature,
              start_x_y_feature,
              end_x_y_feature)
    }

  }

  do.call(rbind, lapply(seq_len(nrows), .features_processing))

}

##FIX ME : need to be optimized
## fix start end x y
.fix_start_end_coor <- function(event_, home_team_id, spadl_cfg =
                                  .settings$spadl_config) {
  if (event_$team_id != home_team_id) {
    event_$start_x <-  spadl_cfg$field_length - event_$start_x
    event_$end_x <-  spadl_cfg$field_length - event_$end_x
    event_$start_y <-  spadl_cfg$field_length - event_$start_y
    event_$end_y <-  spadl_cfg$field_length - event_$end_y
  }
  event_
}


## type id feature
.type_id_feature <- function(events) {
  attr <- "type_id"
  type_id_values <- events$type_id
  .bind_columns_features(attr = attr, values = type_id_values)
}

## result id feature
.result_id_feature <- function(events) {
  attr <- "result_id"
  result_id_values <- events$result_id
  .bind_columns_features(attr = attr, values = result_id_values)
}

## bofypart id feature
.body_part_id_feature <- function(events) {
  attr <- "body_part_id"
  body_part_id_values <- events$result_id
  .bind_columns_features(attr = attr, values = body_part_id_values)
}

.start_x_y_feature <- function(events) {

  out_ <- tibble()
  start_names <- c("start_x", "start_y")
  for (idx in seq_len(length(start_names))) {
    attr <- start_names[idx]
    values <- events[, attr, with = FALSE]
    ## columns bind type actions
    if (nrow(out_) != 0)
      out_ <- out_ %>% cbind(.bind_columns_features(attr, values))
    else
      out_ <- .bind_columns_features(attr, values)
  }
  out_
}

.end_x_y_feature <- function(events) {

  out_ <- tibble()
  end_names <- c("end_x", "end_y")
  for (idx in seq_len(length(start_names))) {
    attr <- end_names[idx]
    values <- events[, attr, with = FALSE]
    ## columns bind type actions
    if (nrow(out_) != 0)
      out_ <- out_ %>% cbind(.bind_columns_features(attr, values))
    else
      out_ <- .bind_columns_features(attr, values)
  }
  out_
}


## type_name feature
.type_name_feature <- function(events,
                               type = c("type", "result", "body_part"),
                               spadl_cfg = .settings$spadl_config) {
  if (type == "type")
    type_names <- spadl_cfg$actiontypes$action_name
  else if (type == "result")
    type_names <- spadl_cfg$results$result_name
  else
    type_names <- spadl_cfg$bodyparts$bodypart_name

  event_type_name <- paste0(type, "_name")
  out_ <- tibble()
  ## loop into action_names
  for (idx in seq_len(length(type_names))) {
    ## build the name action
    attr <- paste0(type, "_", type_names[idx])
    values <- c()
    for (idx_event in seq_len(nrow(events))) {
      check_type_name <- FALSE
      if (events[idx_event, event_type_name,
                 with = FALSE] == type_names[idx])
        check_type_name <- TRUE
      values <- c(values, check_type_name)
    }
    ## columns bind type actions
    if (nrow(out_) != 0)
      out_ <- out_ %>% cbind(.bind_columns_features(attr, values))
    else
      out_ <- .bind_columns_features(attr, values)
  }
  out_
}


## generic features function to bind 2 previous event with the current one
.bind_columns_features <- function(attr, values, nb_events = 3) {
  out_ <- tibble()
  for (idx in seq_len(nb_events)) {
    ## column name by id
    column_name <- paste0(attr, "_a", as.character(idx))
    out_[1, column_name] <- values[idx]
  }
  out_
}
