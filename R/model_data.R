#' @importFrom tidyr drop_na
.get_model_data <- function(spadl_df, nb_prev_actions = 3L, nr_actions = 10L,
                            labels = TRUE, spadl_type = c("standard", "atomic")) {
    spadl_type <- match.arg(spadl_type)
    feats <- target <- NULL
    if (spadl_type == "standard")
        c(feats, target) %<-%
            socceraction_py$vaep$prod$get_model_data(spadl_df,
                                                     as.integer(nb_prev_actions),
                                                     as.integer(nr_actions))
    else if (spadl_type == "atomic")
        c(feats, target) %<-%
            socceraction_py$atomic$vaep$prod$get_model_data(spadl_df,
                                                            as.integer(nb_prev_actions),
                                                            as.integer(nr_actions))

    if (!is.null(target))
        feats <- cbind.data.frame(feats, target)

    ## add event id
    feats$event_id <- spadl_df$event_id

    tidyr::drop_na(feats)
}

.get_vaep_values <- function(spadl_df, type = c("standard", "atomic")) {
    type <- match.arg(type)
    switch(type,
           "standard" = socceraction_py$vaep$formula$value_prod(spadl_df),
           "atomic" = socceraction_py$atomic$vaep$formula$value_prod(spadl_df)
           )
}

.convert_spadl_to_atomic <- function(spadl_df) {
    socceraction_py$atomic$spadl$convert_to_atomic(spadl_df)
}
