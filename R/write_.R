#' Write a data frame to a file with extra info
#'
#' @details Writes an .Rds file using saveRDS with xz compression
#'   and an companion .inf.md file with extra information for the data.
#'
#' @param object A data frame to be saved.
#' @param file A file name to use without the extension.
#' @param contact Contact information for the data set.
#'
#' @export
#'
write_RDS <- function(object, file, contact = "<natsisthanasis@gmail.com>") {
    library(pander)
    library(funr)

    scr_name <- try(funr::sys.script())
    if (file.exists(scr_name)) { Script.Name <- scr_name }

    file = sub(".Rds$", "" , file, ignore.case = T )

    rdsfile = file.path(paste0(file,".Rds"))
    inffile = file.path(paste0(file,".inf.md"))

    pander::panderOptions('table.continues', "")
    # pander::panderOptions('table.style', "simple")
    pander::panderOptions('table.style', "rmarkdown")
    pander::panderOptions('table.emphasize.rownames', FALSE)

    ## save data
    saveRDS( object   = object,
             file     = rdsfile,
             compress = "xz")
    cat("Written: ", rdsfile, "\n" )

    ## write some data info
    capture.output({
        cat(sprintf("<!-- This is a markdown file. -->\n\n"))

        cat(sprintf("\n General info.\n---------------\n\n"))

        try(cat(sprintf("Object name:    %s      \n", deparse(substitute(object)) )))
        try(cat(sprintf("Date written:   %s UTC  \n", file.info(rdsfile)[,"mtime"] )))
        try(cat(sprintf("Data file:      %s      \n", rdsfile    )))
        try(cat(sprintf("Data file size: %s (%s) \n", gdata::humanReadable(file.size(rdsfile)),
                        file.size(rdsfile) )))
        try(cat(sprintf("Info file:      %s      \n", inffile    )))
        try(cat(sprintf("Script name:    %s      \n", Script.Name)), silent = T)
        try(cat(sprintf("User@Host:      %s@%s   \n", Sys.info()["login"],Sys.info()["nodename"]    )))
        try(cat(sprintf("Contact:        %s      \n", contact    )))

        cat(sprintf("\n\n Data structure.\n-----------------\n\n"))

        cat(sprintf("```\n"))
        str(object)
        cat(sprintf("```\n"))

        cat(sprintf("\n\n Data quality.\n---------------"))
        try(pander::pander(rbind(
            Values = lapply(object, function(x) sum(is.finite(x))),
            INFs   = lapply(object, function(x) sum(is.infinite(x))),
            NAs    = lapply(object, function(x) sum(is.na(x)))
        )                 )     )

        cat(sprintf("\n Data Summary.\n---------------"))

        ## for the whole object
        try(pander::pander(summary( object )))

        ## for objects within objects
        try(pander::pander( lapply(object, function(x) summary(x)) ))



    }, file = inffile  )

    cat("Written: ", inffile, "\n" )

}



#' Default method to write dat files of data
#'
#' @param object  Object with data
#' @param file    File to write to
#' @param contact Contact information of data
#' @return Writes a file with data and a corresponding info (md) file.
#'
#' @family reports functions
#' @export
write_dat <- function(object, file, contact = "<natsisthanasis@gmail.com>") {

    scr_name <- try(funr::sys.script())
    if (file.exists(scr_name)) { Script.Name <- scr_name }

    file = sub(".dat$", "" , file, ignore.case = T )

    datfile = file.path(paste0(file,".dat"))
    inffile = file.path(paste0(file,".inf.md"))

    pander::panderOptions('table.continues', "")
    # pander::panderOptions('table.style', "simple")
    pander::panderOptions('table.style', "rmarkdown")
    pander::panderOptions('table.emphasize.rownames', FALSE)

    write.table( x            = format( object),
                 file         = datfile,
                 append       = FALSE,
                 quote        = FALSE,
                 sep          = " ;  ",
                 eol          = "\r\n",
                 na           = "NA",
                 dec          = ".",
                 row.names    = FALSE,
                 col.names    = TRUE,
                 qmethod      = c("escape", "double"),
                 fileEncoding = "")

    cat("Written: ", datfile, "\n" )

    ## write some data info
    capture.output({
        cat(sprintf("<!-- This is a markdown file. -->\n\n"))

        cat(sprintf("\n General info.\n---------------\n\n"))

        try(cat(sprintf("Object name:    %s      \n", deparse(substitute(object)) )))
        try(cat(sprintf("Date written:   %s UTC  \n", file.info(datfile)[,"mtime"] )))
        try(cat(sprintf("Data file:      %s      \n", datfile    )))
        try(cat(sprintf("Data file size: %s (%s) \n", gdata::humanReadable(file.size(rdsfile)), file.size(rdsfile) )))
        try(cat(sprintf("Info file:      %s      \n", inffile    )))
        try(cat(sprintf("Script name:    %s      \n", Script.Name)), silent = T)
        try(cat(sprintf("User@Host:      %s@%s   \n", Sys.info()["login"],Sys.info()["nodename"]    )))
        try(cat(sprintf("Contact:        %s      \n", contact    )))

        cat(sprintf("\n\n Data structure.\n-----------------\n\n"))

        cat(sprintf("```\n"))
        str(object)
        cat(sprintf("```\n"))

        cat(sprintf("\n\n Data quality.\n---------------"))
        try(pander::pander(rbind(
            Values = lapply(object, function(x) sum(is.finite(x))),
            INFs   = lapply(object, function(x) sum(is.infinite(x))),
            NAs    = lapply(object, function(x) sum(is.na(x)))
        )                 )     )

        cat(sprintf("\n Data Summary.\n---------------"))

        ## for the whole object
        try(pander::pander(summary( object )))

        ## for objects within objects
        try(pander::pander( lapply(object, function(x) summary(x)) ))

    }, file = inffile  )

    cat("Written: ", inffile, "\n" )

}
