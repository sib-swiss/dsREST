#'@export
loadApp <- function(middleW){
  app <-  Application$new(content_type = "application/json", middleware = list(middleW))

  # load the enpoint descriptions in a sandbox env:
  temp <- new.env()
  for(p in list.files(config$endpointDir, full.names = TRUE)){
    source(p, local = temp)
  }

  # and attach them to the app (name of endpoint in the file must match the path)
  for(endPath in ls(temp)){
    endp <- get(endPath, temp)
    endp$path <- paste0('/', endPath)
    do.call(app$add_route, endp)
  }
  rm(temp)
  return(app)
}
