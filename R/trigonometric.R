

#' Calculate the cosine of an angle given in degrees
#'
#' @param degrees Input angle in degrees
#' @return cos(degrees)
#'
#' @family math
#' @export
#'
cosde <- function(degrees) {
    ## calculate the cos of an angle given in degrees
    cosine <- cos(degrees * pi / 180)
    return(cosine)
}


#' Calculate the sine of an angle given in degrees
#'
#' @param degrees Input angle in degrees
#' @return sin(degrees)
#'
#' @family math
#' @export
#'
sinde <- function(degrees) {
    ## calculate the cos of an angle given in degrees
    sine <- sin(degrees * pi / 180)
    return(sine)
}


#' Calculate the tan of an angle given in degrees
#'
#' @param degrees Input angle in degrees
#' @return tan(degrees)
#'
#' @family math
#' @export
#'
tande <- function(degrees) {
    ## calculate the cos of an angle given in degrees
    tang <- tan(degrees * pi / 180)
    return(tang)
}

