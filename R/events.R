#' @param gameid integer gameId
#' @param events_con  mongo event connection
#' @import data.table
#' @export
.extract_events_from_game <- function(gameid,
                                      events_con = .settings$events_con) {



  ## get events per game
  events_query <- paste0('{"gameId" : ', gameid, "}")
  events <- events_con$find(events_query)

  ## check if retrieved events collection is empty
  if (nrow(events) == 0 | ncol(events) == 0) {
    return(data.table())
  }
  ## qualifiers
  qualifiers <- events$qualifiers

  ## number of events row per game
  nrows <- nrow(events)


  .parse_qualifiers <- function(idx_row) {
    if (is.data.frame(qualifiers))
      qualifiers_ <- .read_qualifiers(qualifiers[idx_row, ])
     else
      qualifiers_ <- .read_qualifiers(qualifiers[[idx_row]])


    list(qualifiers = qualifiers_)
  }

  ## get all qualifiers from events
  qualifiers_ <- do.call(rbind, lapply(1:nrows, .parse_qualifiers))

  ## parse a single event by index
  .parse_single_event <- function(idx_row) {
    assist <- NA
    keypass <- NA

    ## get event by id
    event_ <- events[idx_row, ]

    ## start position of the event
    start_x_ <- event_$x %>% as.numeric()
    start_y_ <- event_$y %>% as.numeric()

    ## TRUE or FALSE outcome
    outcome_ <- event_$outcome %>% as.logical()


    type_id_ <- event_$typeId %>% as.numeric()

    ## minute & seconds of the event
    min_ <- event_$min %>% as.numeric()
    sec_ <- event_$sec %>% as.numeric()

    team_id_ <- event_$teamId %>% as.numeric()
    player_id_ <- event_$playerId %>% as.numeric()

    ## qualifiers as data.frame
    qualifiers_ <- .parse_qualifiers(idx_row)$qualifiers

    ## end position of the event
    end_y_ <- .end_y_value(qualifiers_)
    end_x_ <- .end_x_value(qualifiers_)

    ## keypass or assist if exists
    c(keypass, assist) %<-% .keypasses_assists(event_$typeId)

    ## reformat event as data.table
    data.table(
      gameid = gameid,
      team_id = team_id_,
      player_id = player_id_,
      type_id = type_id_,
      start_x = start_x_,
      start_y = start_y_,
      end_x = end_x_,
      end_y = end_y_,
      min = min_,
      sec = sec_,
      keypass = keypass,
      assist = assist,
      outcome = outcome_
    )
  }

  ## get all events from a given gameid
  events_ <- do.call(rbind, lapply(1:nrows, .parse_single_event))


  ## associate each qualifiers to its event
  cbind(events_,  qualifiers = qualifiers_)
}

#' get keypasses or assists if exists
#' @param type_id  integer  event type
.keypasses_assists <- function(type_id) {
  assist <- FALSE
  keypass <- FALSE

  ## get keypass & assist type ids from opta config
  assist_value <- .settings$opta_config$assist
  keypass_values <- .settings$opta_config$keypass
  if (type_id == assist_value)
    assist <- TRUE
  else if (type_id %in% keypass_values)
    keypass <- TRUE

  c(keypass, assist)
}

#' @param qualifiers  data.frame qualifiers's event
.end_x_value <- function(qualifiers) {
  end_x <- NA

  qualifiers_keys <- names(qualifiers)


  if ("140" %in% qualifiers_keys) {
    end_x <- qualifiers["140"] %>% as.numeric()
  } else if ("146" %in% qualifiers_keys) {
    end_x <- qualifiers["146"] %>% as.numeric()
  } else if ("102" %in% qualifiers_keys) {
    end_x <- 100L
  }

  end_x
}
#' @param qualifiers  data.frame qualifiers's event
.end_y_value <- function(qualifiers) {
  end_y <- NA

  qualifiers_keys <- names(qualifiers)

  if ("141" %in% qualifiers_keys) {
    end_y <- qualifiers["141"] %>% as.numeric()
  } else if ("147" %in% qualifiers_keys) {
    end_y <- qualifiers["147"] %>% as.numeric()
  } else if ("102" %in% qualifiers_keys) {
    end_y <- qualifiers["102"] %>% as.numeric()
  }

  end_y
}




#' read qualifiers as array or as data.frame
#' @param qualifiers  array data.frame data.frame
.read_qualifiers <- function(qualifiers) {
  if (is.null(qualifiers)) {
    return(data.frame())
  }

  if (class(qualifiers) == "list") {
    if (length(qualifiers) == 1) {
      qualifiers <- qualifiers[[1]]
    } else {
      nl <- length(qualifiers)
      ## extract qualifiers names
      q_names <- character()
      for (i in 1:nl) q_names <- c(q_names, names(qualifiers[[i]]))
      q_names <- unique(q_names)

      out <- data.frame()
      for (i in 1:nl) {
        qs <- qualifiers[[i]]
        qs_name <- q_names[which(!q_names %in% names(qs))]
        if (length(qs_name) > 0) {
          for (k in seq_along(qs_name)) qs[[qs_name[k]]] <- NA
        }
        out <- rbind(out, qs)
      }
      qualifiers <- out
    }
  }

  stopifnot(class(qualifiers) == "data.frame")
  if (nrow(qualifiers) == 0 | ncol(qualifiers) == 0) {
    return(data.frame())
  }

  ## remove columns with all NA
  na_keep <- which(sapply(
    seq_len(ncol(qualifiers)),
    function(ind) all(!is.na(qualifiers[, ind]))
  ))
  qualifiers[, na_keep, drop = FALSE]
}
