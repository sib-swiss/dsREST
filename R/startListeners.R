#'@export
startListeners <- function(reqPath, resPath, howMany, confFile,  waitInterval = 5, verbose = FALSE){

  listeners <- list()

  for(i in 1:howMany){
    code <- paste0("suppressMessages(dsREST::listen(", Sys.getpid(),  ",'",confFile, "','", reqPath, "','", resPath, "', verbose = ", verbose,"))")

    listeners[[i]] <- processx::process$new('/usr/bin/Rscript',
                                            c('-e',code), cleanup = FALSE, stderr = '', stdout = '')
    Sys.sleep(waitInterval)
  }
  listeners
}


