#' @param gameid integer gameId
#' @param events_con  mongo event connection
#' @import data.table
#' @export
.extract_events_from_game <- function(gameid,
                                      events_con = .settings$events_con) {



  ## get events per game
  events_query <- paste0('{"gameId" : ', gameid, "}")
  events <- events_con$find(events_query)

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


  .parse_single_event <- function(idx_row) {

    event_ <- events[idx_row, ]
    start_x_ <- event_$x %>% as.numeric()
    start_y_ <- event_$y %>% as.numeric()
    outcome_ <- event_$outcome %>% as.numeric()

    qualifiers_ <- .parse_qualifiers(idx_row)$qualifiers

    end_y_ <- .end_y_value(qualifiers_)
    end_x_ <- .end_x_value(qualifiers_)

    data.table(
      gameid = gameid,
      start_x = start_x_,
      start_y = start_y_,
      end_x = end_x_,
      end_y = end_y_,
      outcome = outcome_
    )
  }

  events_ <- do.call(rbind, lapply(1:nrows, .parse_single_event))



  cbind(events_,  qualifiers = qualifiers_)
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

## read qualifiers as array or as data.frame
#' @param qualifiers  array data.frame
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
