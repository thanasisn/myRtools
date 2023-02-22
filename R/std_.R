
## https://github.com/thanasisn <natsisphysicist@gmail.com>




#' Standard starting part in a script
#'
#' @description   Still have to use 'rm(list = (ls()[ls() != ""]))" before this
#'
#' @param ScriptName Use this if can not resolve name
#' @param outdir     Folder for extra files output
#'
#' @family standard scripting
#' @return
#' @export
#'
std_setenv_v1 <- function( ScriptName = "",
                           outdir     = "" ){
    is_filelock <- require(filelock)

    require(funr,     quietly = TRUE)
    require(filelock, quietly = TRUE)
    ## we always work with UTC
    Sys.setenv(TZ = "UTC")
    ## keep track of execution time
    tic <<- Sys.time()
    ## get script file name
    Script.Name <<- tryCatch({
        sys.script() },
        error = function(e) {
            cat(paste("\nUnresolved script name: ", e),"\n")
            if ( ScriptName != "" ) {
                return(ScriptName)
            } else {
                return("Undefined_R_script_name")
            }
        }
    )
    ## paths from script name
    basepath <- paste0(  sub("\\.R$","", Script.Name) )
    if (outdir != "") {
        basedir  <- paste0(normalizePath(outdir),"/")
        basepath <- paste0( basedir, basename(basepath))
    }
    ## output locations
    if(!interactive()) {
        pdf( file = paste0( basepath, ".pdf") )
        sink(file = paste0( basepath, ".out"),  split=TRUE)
        if (is_filelock) {
            lock( paste0( basepath, ".lock"), timeout = 0)
        }
    }
}



#' Standard end for scripts
#'
#' @family standard scripting
#' @return
#' @export
#'
std_scriptend_v1 <- function(){
    ## keep track of execution time
    tac <<- Sys.time()
    ## print end message
    cat(sprintf("\n%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
}


# std_setenv_v1()
# std_scriptend_v1()
# this is test

