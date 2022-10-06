logout = list(
  method = 'GET',
  FUN = function(req, res){
    rPath <- paste0(resPath, '/', req$cookies[['user']], req$cookies[['sid']])
    unlink(rPath, recursive = TRUE, force = TRUE)
    res$set_body('OK')
  }
)
