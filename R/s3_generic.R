#' Convert formatted events to SPADL formatted (generic)
#'
#' Convert formatted events to SPADL formatted
#'
#' @param object tibble such as the object created by
#'     \code{.opta_events_from_game()}
#' @param ... extra parameters currently used
#' @return \code{tibble} representing SPADL info.
#' @export
#' @name spadl_conversion
convert_events_to_spadl <- function(object, ...) {
    UseMethod("convert_events_to_spadl", object)
}
