
## https://github.com/thanasisn <lapauththanasis@gmail.com>

#### Functions to store data





#' Gather meta data for an R object
#'
#' @param object   Object to get metadata for
#' @param contact  Contact information for the data
#' @param notes    Notes on the data
#'
#' @return         List with names and values of the metadata
#' @family report functions
#' @export
#'
object_metadata <- function(object,
                            contact = contact,
                            notes   = NA){
    require(funr, quietly = TRUE, warn.conflicts = FALSE)

    scr_name <- try(sys.script(),TRUE)
    if (file.exists(scr_name)) {
        Script.Name <- scr_name
    } else {
        Script.Name <- "Script name could not be resolved"
    }
    meta_obj <- list()
    meta_obj$Object_name <- deparse(substitute(object))
    meta_obj$Meta_date   <- Sys.time()
    meta_obj$Script      <- Script.Name
    meta_obj$User        <- Sys.info()["login"][[1]]
    meta_obj$Host        <- Sys.info()["nodename"][[1]]
    meta_obj$Contact     <- contact
    meta_obj$Notes       <- notes
    meta_obj$Size        <- format(object.size(object), units = "B" )

    return(meta_obj)
}



markdown_output <- function(object,
                            outfile  = outfile,
                            infofile = infofile,
                            contact  = contact,
                            notes    = NA) {
    require(pander, quietly = TRUE, warn.conflicts = FALSE)
    require(funr,   quietly = TRUE, warn.conflicts = FALSE)
    require(gdata,  quietly = TRUE, warn.conflicts = FALSE)

    panderOptions('table.continues', "")
    # panderOptions('table.style', "simple")
    panderOptions('table.style', "rmarkdown")
    panderOptions('table.emphasize.rownames', FALSE)

    if (!exists("meta_obj")){
        meta_obj             <- object_metadata(object  = object,
                                                contact = contact,
                                                notes   = notes)
        meta_obj$Object_name <- deparse(substitute(object))
    }

    cat(sprintf("<!-- This is a markdown file. -->\n\n"))

    cat(sprintf("\n General info.\n---------------\n\n"))

    (cat(sprintf("Object name:    %s      \n", meta_obj$Object_name )))
    (cat(sprintf("Date written:   %s UTC  \n", file.info(outfile)[,"mtime"] )))
    (cat(sprintf("Data file:      %s      \n", outfile    )))
    (cat(sprintf("Data file size: %s (%s) \n", humanReadable(file.size(outfile)), file.size(outfile) )))
    (cat(sprintf("Size in memory: %s      \n", meta_obj$Size )))
    (cat(sprintf("Info file:      %s      \n", infofile                                   )))
    (cat(sprintf("Script name:    %s      \n", meta_obj$Script                            )))
    (cat(sprintf("User@Host:      %s@%s   \n", Sys.info()["login"],Sys.info()["nodename"] )))
    (cat(sprintf("Contact:        %s      \n", contact                                    )))
    (cat(sprintf("Notes:          %s      \n", notes                                      )))

    cat(sprintf("\n\n Data structure.\n-----------------\n\n"))

    cat(sprintf("```\n"))
    str(object)
    cat(sprintf("```\n"))

    cat(sprintf("\n\n Data quality.\n---------------"))
    try(pander(rbind(
        Values = lapply(object, function(x) sum(is.finite(x))),
        INFs   = lapply(object, function(x) sum(is.infinite(x))),
        NAs    = lapply(object, function(x) sum(is.na(x)))
    )))

    cat(sprintf("\n Data Summary.\n---------------"))

    ## for the whole object
    try(pander(summary( object )))

    ## for objects within objects
    try(pander( lapply(object, function(x) summary(x)) ))

}





#' Save data with multiple output format and extra metadata
#'
#' @param object  Object with data (not all types will work)
#' @param file    Prefix of output file. The extension will be added by the type
#' @param contact Contact information of data
#' @param notes   Notes on the data
#' @param clean   If `TRUE` don't write .inf.md files
#' @param type    A vector of types to write. One of c("Rds","dat","prqt")
#'
#' @return        Writes files with data and a corresponding info (md) file.
#' @family        write functions
#' @example       writeDATA(iris, "~/iris", type = c("Rds","dat","prqt") )
#' @export
#'
writeDATA <- function(object, file,
                      contact = "<lapauththanasis@gmail.com>",
                      notes   = NA,
                      clean   = FALSE,
                      type    = c("Rds") ) {
    require(pander, quietly = TRUE, warn.conflicts = FALSE)
    require(funr,   quietly = TRUE, warn.conflicts = FALSE)
    ## definitions
    knowntypes <- c("Rds","dat","prqt")
    type       <- unique(type)
    ## check for expected types
    if (! all(toupper(type) %in% toupper(knowntypes))){
        warning(paste("Unkown type:",type[!toupper(type)%in%toupper(knowntypes)]))}
    ## keep only known types
    type <- type[toupper(type)%in%toupper(knowntypes)]
    ## recreate output prefix
    basenm <- basename(file)
    basenm <- sub("\\..*$", "" , basenm )
    folder <- dirname(file)
    prefix <- path.expand(paste0(folder,"/",basenm))
    ## output target folder check
    if ( ! dir.exists(folder) ) {
        stop(paste("Output folder dosn't exist:",folder)) }
    ## get metadata for object
    meta_obj             <<- object_metadata(object  = object,
                                             contact = contact,
                                             notes   = notes)
    meta_obj$Object_name <<- deparse(substitute(object))

    ####  Output multiple data files according to options ####
    for (at in type) {
        outfile <- file.path(paste0(prefix,".",at))
        at      <- toupper(at)
        if        (at == "PRQT")   {
            if (! require("arrow",quietly = TRUE, warn.conflicts = FALSE) ) {
                warning("Missing package 'arrow'")
                cat(paste("SKIP:",outfile),"\n\n")
                next()
            }
            if (!is.data.frame(object)) {
                warning(paste("arrow::parquet >> ",
                              meta_obj$Object_name,
                              " << was converted to data.frame"))
                object <- as.data.frame(object)
            }
            write_parquet( object,
                           sink        = outfile,
                           compression = 'zstd',
                           compression_level = 9 )
            cat("Written: ", outfile, "\n\n" )


        } else if (at == "DAT")   {
            write.table( x            = format( object),
                         file         = outfile,
                         append       = FALSE,
                         quote        = FALSE,
                         sep          = " ;  ",
                         eol          = "\r\n",  ## for unfortunate people with windows
                         na           = "NA",
                         dec          = ".",
                         row.names    = FALSE,
                         col.names    = TRUE,
                         qmethod      = c("escape", "double"),
                         fileEncoding = "")
            cat("Written: ", outfile, "\n\n" )

            ## todo apply best compression with bash script

        } else if (at == "RDS" ) {
            ## this is only for RDS output
            # attributes( object ) <- c(
            #     attributes( object ),
            #     meta_obj
            # )
            ## save data
            saveRDS( object   = object,
                     file     = outfile,
                     compress = "xz")
            cat("Written: ", outfile, "\n\n" )

        }
    }

    ####  Write some extra data info  ####
    if (!clean) {
        infofile <- file.path(paste0(prefix,".inf.md"))
        capture.output(
            markdown_output(object,
                            outfile  = outfile,
                            contact  = contact,
                            notes    = notes,
                            infofile = infofile),
            file = infofile  )
        cat("Written: ", infofile, "\n\n" )
    }
}




#' Default method to write dat files of data
#'
#' @param object  Object with data
#' @param file    File to write to
#' @param contact Contact information of data
#' @param notes   Notes on the data
#' @param clean   If `TRUE` don't write .inf.md files
#'
#' @note          It uses the command
#'                writeDATA(object = object, file = file, contact = contact, notes = notes, clean = clean, type = "dat")
#'
#' @return Writes a file with data and a corresponding info (md) file.
#'
#' @family write functions
#' @export
write_dat <- function(object,
                      file,
                      contact = "<lapauththanasis@gmail.com>",
                      notes   = NA,
                      clean   = FALSE ) {

    writeDATA(object  = object,
              file    = file,
              contact = contact,
              notes   = notes,
              clean   = clean,
              type    = "dat")
}


#' Write a data frame to an Rds file with extra info
#'
#' @details Writes an .Rds file using saveRDS with xz compression
#'   and an companion .inf.md file with extra information for the data.
#'
#' @param object  A data frame to be saved.
#' @param file    A file name to use without the extension.
#' @param contact Contact information for the data set.
#' @param notes   Notes on the data
#' @param clean   If `TRUE` don't write .inf.md files
#'
#' @note          It uses the command
#'                writeDATA(object = object, file = file, contact = contact, notes = notes, clean = clean, type = "Rds")
#'
#' @family write functions
#' @export
#'
write_RDS <- function(object, file,
                      contact = "<lapauththanasis@gmail.com>",
                      notes   = NA,
                      clean   = FALSE) {

    writeDATA(object  = object,
              file    = file,
              contact = contact,
              notes   = notes,
              clean   = clean,
              type    = "Rds")

}



#' Default method to write parquet files of data with arrow
#'
#' @param object  Object with data
#' @param file    File to write to
#' @param contact Contact information of data
#' @param notes   Notes on the data
#' @param clean   If `TRUE` don't write .inf.md files
#'
#' @note          It uses the command
#'                writeDATA(object = object, file = file, contact = contact, notes = notes, clean = clean, type = "prqt")
#'
#' @return Writes a file with data and a corresponding info (md) file.
#'
#' @family write functions
#' @export
write_prqt <- function(object,
                      file,
                      contact = "<lapauththanasis@gmail.com>",
                      notes   = NA,
                      clean   = FALSE ) {

    writeDATA(object  = object,
              file    = file,
              contact = contact,
              notes   = notes,
              clean   = clean,
              type    = "prqt")
}







#
# writeDATA(iris, "~/iris.jh", type    = c("prqt","Rds","dat","prqt") )
# write_RDS(iris, "~/iris.jh")
# write_dat(iris, "~/iris.jh")
# write_prqt(iris, "~/iris.jh")
