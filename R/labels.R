#' convert SPADL events to labels
#'
#' @param events events from SPADL
#' @param fixtures_con fixtures db connection
#' @importFrom data.table
#' @return \code{tibble} representing labels details.
#' @export
.spadl_to_labels <- function(events) {
  ## if spadl_events empty
  if (nrow(events) == 0)
    return(tibble())


  ## score, condede, goals from shots  labels
   events %>% .scores_concedes_gshots
}

.scores_concedes_gshots <- function(events, nb_lag = 9) {

  ## get goals and owngoals actions
  goals <- grepl("shot", events$type_name) & events$result_name == "success"
  owngoals <- grepl("shot", events$type_name) & events$result_name == "owngoal"
  teams_id <- events$team_id

  ## shift value by 9 to get 10 actions
  shifted_goals <- goals %>% .shift_values(attr = "goals")
  shifted_owngoals <- owngoals %>% .shift_values(attr = "owngoals")
  shifted_team_id <- teams_id %>% .shift_values(attr = "team_id")
  goals_from_shots <- goals

  for (idx in seq_len(nb_lag)) {
    ## scores actions
    s_goals_ <- shifted_goals[[idx]] & (shifted_team_id[[idx]] == teams_id)
    s_own_goals_ <- shifted_owngoals[[idx]] &
      (shifted_team_id[[idx]] != teams_id)

    ## concede actions
    c_goals_ <- shifted_owngoals[[idx]] & (shifted_team_id[[idx]] != teams_id)
    c_own_goals_ <- shifted_owngoals[[idx]] &
      (shifted_team_id[[idx]] == teams_id)

    ## check which is true
    goals <- goals | s_goals_ | s_own_goals_
    owngoals <- owngoals | c_goals_ | c_own_goals_

  }

  tibble(scores = goals,
         concedes = owngoals,
         goals_from_shots = goals_from_shots)
}



.shift_values <- function(attr, values, nb_lag = 9) {
  out_ <- list()
  for (idx in seq_len(nb_lag)) {
    shifted_values <- values %>% data.table::shift(-idx, type = "lag")
  ## column name by id
  column_name <- paste0(attr, "_", as.character(idx))
  out_[[column_name]] <- shifted_values
  }

  out_
}
