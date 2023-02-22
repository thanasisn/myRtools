
## https://github.com/thanasisn <natsisphysicist@gmail.com>

#### Physical conversions

#' Convert Miles per hour to meters per second
#'
#' @param Miles_per_hour   Speed in Miles per hour
#'
#' @return                 Speed in meters per second
#' @export
#'
MpH_to_mps <- function( Miles_per_hour ) {
    # 1 mile =  1609.344 meter
    # 1609.344 / 3600
    return(Miles_per_hour * 0.44704)
}

#' Convert pressure from InHg to mbar
#'
#' @param Inches_Hg Inches of Hg
#'
#' @return          Pressure in millibars
#' @export
#'
InHg_to_mbar <- function( Inches_Hg ) {
    # 1 inch hg = 33.863886 mbar
    return(Inches_Hg * 33.863886)
}

#' Convert pressure from InHg to Pascals
#'
#' @param Inches_Hg Inches of Hg
#'
#' @return          Pressure in pascals
#' @export
#'
InHg_to_Pa <- function( Inches_Hg ) {
    # 1 inch hg = 3386.3886 pascal
    return(Inches_Hg * 3386.3886)
}

#'  Convert inches to millimeters
#'
#' @param Inches Length in inches
#'
#' @return       Length in millimeters
#' @export
#'
In_to_mm <- function( Inches ) {
    # 1 Inch = 25.4 mm
    return(Inches * 25.4)
}

#' Convert Fahrenheit to Celsius
#'
#' @param T.fahrenheit Temperature in Fahrenheit
#'
#' @return             Temperature in Celsius
#' @export
#'
fahrenheit_to_celsius <- function (T.fahrenheit) {
    T.celsius <- (5/9) * (T.fahrenheit - 32)
    return(T.celsius)
}

#' Convert Celsius to kelvin
#'
#' @param T.celsius Temperature in Celsius
#'
#' @return          Temperature in kelvin
#' @export
#'
celsius_to_kelvin <- function (T.celsius) {
    T.kelvin <- T.celsius + 273.15
    return(T.kelvin)
}

#' Convert Celsius to Fahrenheit
#'
#' @param T.celsius Temperature in Celsius
#'
#' @return          Temperature in Fahrenheit
#' @export
#'
celsius_to_fahrenheit <- function (T.celsius) {
    T.fahrenheit <- (9/5) * T.celsius + 32
    return(T.fahrenheit)
}
