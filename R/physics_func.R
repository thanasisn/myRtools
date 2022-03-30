
## https://github.com/thanasisn <lapauththanasis@gmail.com>

#### Physics functions


#' Rayleigh scattering cross section
#'
#' @param lamda_nm light wavelength in nanometers
#'
#' @return         Absorption coefficient at wavelength
#' @export
#' @note           by: Hansen - Travis (1974)
#'
csR <- function(lamda_nm) {
    lambda_micron <- lamda_nm / 1000
    return( 0.008569 * lambda_micron^(-4) * ( 1 + 0.0113 * lambda_micron^(-2) + 0.00013*lambda_micron^(-4) ) )
}

