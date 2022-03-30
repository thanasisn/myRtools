
## https://github.com/thanasisn <lapauththanasis@gmail.com>

#### Format for better human display

#' Get a nice string for the difference of two dates
#'
#' @details The difference of dates is computed with difftime in seconds.
#'
#' @param etime (POSIX date) The end time.
#' @param stime (POSIX date) The start time.
#' @param type  The output format. "h" "93:29:43.758", "d" "3 21:34:17.072", "s" "336936.990"
#'
#' @return      A string of time duration formatted as time
#' @export
#'
fmt_difftime <- function(etime, stime, type) {
    sss     <- as.numeric(difftime(etime, stime, units = "secs"))
    seconds <- sss %% 60
    minutes <- sss %/% 60 %% 60
    hours   <- sss %/% 60 %/% 60
    hoursd  <- hours  %% 24
    days    <- hours %/% 24

    if        ( type == "h") {
        output = sprintf("%02d:%02d:%06.3f", hours, minutes, seconds)
    } else if ( type == "d" ) {
        output = sprintf("%s %02d:%02d:%06.3f", days, hoursd, minutes, seconds)
    } else if ( type == "s" ) {
        output = sprintf("%06.3f", sss )
    }
    return(output)
}

#' Get a nice string for seconds as formatted time
#'
#' @param seconds Time duration in seconds
#' @param type    The output format. "h" "93:29:43.758", "d" "3 21:34:17.072", "s" "336936.990"
#'
#' @return        A string of time duration formatted as time
#' @export
#'
fmt_secs <- function(seconds, type) {
    sss    <- seconds
    secs   <- sss %% 60
    mins   <- sss %/% 60 %% 60
    hours  <- sss %/% 60 %/% 60
    hoursd <- hours  %% 24
    days   <- hours %/% 24

    if        ( type == "h" ) {
        output = sprintf("%02d:%02d:%06.3f", hours, mins, secs)
    } else if ( type == "d" ) {
        output = sprintf("%s %02d:%02d:%06.3f", days, hoursd, mins, secs)
    } else if ( type == "s" ) {
        output = sprintf("%06.3f", sss )
    }
    return(output)
}


