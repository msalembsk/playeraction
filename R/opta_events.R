#' @import tibble mongoTools
#' @importFrom dplyr left_join arrange select
#' @importFrom aroundthegoal to_l1
#' @export
.opta_events_from_game <- function(game_id,
                                   events_con = .settings$events_con,
                                   keypass_con =
                                       .settings[["playerKeyPasses_con"]],
                                   opta_cfg = .settings$opta_config) {

    ## get events per game
    keys <- list(gameId = game_id)
    events_query <- buildQuery(names(keys), keys)
    events <- events_con$find(events_query)

    ## check if retrieved events collection is empty
    if (nrow(events) == 0)
        return(tibble())

    ## extract key pass
    out <- list(pass_type = 1, eventId = 1, "_id" = 0)
    qo <- buildQuery(names(out), out)
    key_pass <- keypass_con$find(events_query, qo)
    if (nrow(key_pass) > 0)
        events <- left_join(events, key_pass, by = "eventId")

    ## number of events row per game
    nrows <- nrow(events)

    .parse_qualifiers <- function(qualifiers) {
        if (is.data.frame(qualifiers))
            .read_qualifiers(qualifiers[1, ])
        else
            .read_qualifiers(qualifiers)
    }

    ## parse a single event by index
    .parse_single_event <- function(idx_row) {
        ## get event by id
        event_ <- events[idx_row, ]
        qualifiers_ <- .parse_qualifiers(event_[["qualifiers"]])

        ## start position of the event
        start_x_ <- as.numeric(event_$x)
        start_y_ <- as.numeric(event_$y)

        ## TRUE or FALSE outcome
        outcome_ <- as.logical(event_$outcome)

        type_id_ <- as.integer(event_[["typeId"]])
        event_id <- as.numeric(event_[["eventId"]])

        ## minute & seconds of the event
        min_ <- as.integer(event_$min)
        sec_ <- as.integer(event_$sec)
        period_id_ <- as.integer(event_[["periodId"]])

        team_id_ <- as.integer(event_[["teamId"]])
        player_id_ <- as.integer(event_[["playerId"]])

        ## end position of the event
        end_x_ <- .get_end_coordinate(qualifiers = qualifiers_,
                                      q_pass_end =
                                          opta_cfg[["Q_pass_end_x"]],
                                      q_blocked =
                                          opta_cfg[["Q_blocked_x"]],
                                      q_goal_mouth =
                                          opta_cfg[["Q_goal_mouth_y"]],
                                      use_goal_mouth = FALSE)
        if (is.na(end_x_))
            end_x_ <- start_x_
        end_y_ <- .get_end_coordinate(qualifiers = qualifiers_,
                                      q_pass_end =
                                          opta_cfg[["Q_pass_end_y"]],
                                      q_blocked =
                                          opta_cfg[["Q_blocked_y"]],
                                      q_goal_mouth =
                                          opta_cfg[["Q_goal_mouth_y"]],
                                      use_goal_mouth = TRUE)
        if (is.na(end_y_))
            end_y_ <- start_y_

        ## keypass or assist if exists
        pass_type <- event_$pass_type
        assist_ <- keypass_ <- FALSE
        if (!is.na(pass_type)) {
            if (pass_type == "key")
                keypass_ <- TRUE
            else if (pass_type == "assisst")
                assist_ <- TRUE
        }

        ## reformat event as data.frame
        tibble(game_id = game_id,
               event_id = event_id,
               type_id = type_id_,
               period_id = period_id_,
               minute = min_,
               second = sec_,
               player_id = player_id_,
               team_id = team_id_,
               outcome = outcome_,
               start_x = start_x_,
               start_y = start_y_,
               end_x = end_x_,
               end_y = end_y_,
               assist = assist_,
               keypass = keypass_,
               qualifiers = to_l1(qualifiers_)
               )
    }

    ## get all events from a given game_id
    res <- do.call(rbind, lapply(seq_len(nrows), .parse_single_event))

    class(res) <- c("opta_events", class(res))
    res
}

.get_end_coordinate <- function(qualifiers,
                                q_pass_end, q_blocked, q_goal_mouth,
                                use_goal_mouth = TRUE) {
    res <- NA

    qualifiers_keys <- names(qualifiers)

    if (q_pass_end %in% qualifiers_keys)
        res <- as.numeric(qualifiers[q_pass_end])
    else if (q_blocked %in% qualifiers_keys)
        res <- as.numeric(qualifiers[q_blocked])
    else if (q_goal_mouth %in% qualifiers_keys) {
        if (use_goal_mouth)
            res <- as.numeric(qualifiers[q_goal_mouth])
        else
            res <- 100
    }

    res
}

.read_qualifiers <- function(qualifiers) {
    if (is.null(qualifiers))
        return(tibble())

    if (class(qualifiers) == "list") {
        if (length(qualifiers) == 1)
            qualifiers <- qualifiers[[1]]
        else {
            nl <- length(qualifiers)
            ## extract qualifiers names
            q_names <- character()
            for (i in seq_len(nl))
                q_names <- c(q_names, names(qualifiers[[i]]))
            q_names <- unique(q_names)

            out <- data.frame()
            for (i in seq_len(nl)) {
                qs <- qualifiers[[i]]
                qs_name <- q_names[which(!q_names %in% names(qs))]
                if (length(qs_name) > 0) {
                    for (k in seq_along(qs_name))
                        qs[[qs_name[k]]] <- NA
                }
                out <- rbind(out, qs)
            }
            qualifiers <- out
        }
    }

    stopifnot(class(qualifiers) == "data.frame")
    if (nrow(qualifiers) == 0 | ncol(qualifiers) == 0)
        return(data.frame())

    ## remove columns with all NA
    na_keep <- which(sapply(
        seq_len(ncol(qualifiers)),
        function(ind) all(!is.na(qualifiers[, ind]))
    ))
    qualifiers[, na_keep, drop = FALSE]
}
