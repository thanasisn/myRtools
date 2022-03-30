
## https://github.com/thanasisn <lapauththanasis@gmail.com>

#### Functions to clean data


#' Clean names of an object
#'
#' @param df  An object that has `names()`
#'
#' @return    The same object with cleaner names
#' @export
#' @note      This will remove all spaces and dots from files names
#'            and will substitute them with underscores.
#'
clean_names <- function(df) {
    ## replace all spaces
    names(df) <- gsub(" ", "_", names(df))
    ## replace all dots
    names(df) <- gsub("\\.", "_", names(df))
    ## replace duplicates underscores
    names(df) <- gsub("_+", "_", names(df))
    ## remove underscores from start and end
    names(df) <- sub("^_+", "", names(df))
    names(df) <- sub("_+$", "", names(df))
    return(df)
}
