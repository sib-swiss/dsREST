#'@export
startListeners <- function(reqPath, resPath, howMany, confFile, waitInterval = 5){

  listeners <- list()

  for(i in 1:howMany){
    code <- paste0("suppressMessages(dsREST::listen('",confFile, "','", reqPath, "','", resPath, "'))")

    listeners[[i]] <- processx::process$new('/usr/bin/Rscript',
                                            c('-e',code), cleanup = TRUE, stderr = '', stdout = '')
    Sys.sleep(waitInterval)
  }
  listeners
}
