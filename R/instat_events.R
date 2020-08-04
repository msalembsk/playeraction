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

  ## number of events row per game
  nrows <- nrow(events)

  action_types <- instat_cfg$action_types

  ## get only useful events
  events <- events[which(events$generic_action_type_name %in% action_types), ]

  ## fill missing bodypart with foot
  events$body_id[is.na(events$body_id)] <- 1L ## foot ID
  ## parse a single event by index
  .parse_single_event <- function(idx_row) {
    ## get event by id
    event_ <- events[idx_row, ]
    spadl_action_name <- .generic_action_family(event_)


    tibble(action_name = spadl_action_name)
  }

  ## get all events from a given game_id
  res <- do.call(rbind, lapply(seq_len(nrows), .parse_single_event))

  res
}


.pass_action_family <- function(pass_event_,
                                 instat_cfg = .settings$instat_config) {
  ## check standart action for the right real action
  instat_action <- pass_event_$standart_name

  throw_in <- instat_action %in% instat_cfg$throw_in
  ## FIXME : check freekick short or crossed
  freekick <- instat_action %in% instat_cfg$freekick
  ## short corner else check cross action + corner attack function
  ## not sure
  corner_short <- instat_action %in% instat_cfg$corner
  action_name <-
    dplyr::case_when(throw_in ~ "throw_in",
                     freekick ~ "freekick",
                     corner_short ~ "corner_short",
                     TRUE ~ "pass")
  action_name
}



.generic_action_family <- function(event_,
                                    instat_cfg = .settings$instat_config) {
  action_type <- event_$generic_action_type_name
  action_subtype_name <- event_$action_subtype_name

  action_name <- NULL
  if (action_type == "Pass"  & action_subtype_name %in% instat_cfg$action_pass)
    action_name <- .pass_action_family(event_)

  action_name
}
