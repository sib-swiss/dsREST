descriptivestats = list(
  method = 'POST',
  FUN = function(req,res){

    #params <- req$parameters_query

    if(is.null(params$type)){
      params$type <- 'combine'
    }
    rPath <- paste0(resPath, '/', req$cookies[['user']], req$cookies[['sid']])
    params <- jsonlite::fromJSON(req$body)
    if(is.null(params$type)){
      params$type <- 'combine'
    }
    covars <- params$covariables
    vars <- params$variables
    allvars <- c(covars, vars)
    quants <- sapply(allvars, function(var){
        x <- qCommand(reqQ, rPath, message = list(fun = 'sliceNdice',
                                             args = list( req$cookies[['sid']], 'ds.quantileMean', var, params$type, params$datasets), timeout = 500))
        jsonlite::fromJSON(x$message, simplifyDataFrame = FALSE, simplifyMatrix = TRUE ) %>% unlist
        }, simplify = FALSE)
    if(length(allvars) == 1){
      names(quants) <- allvars
    }
   # heatmaps <- lapply(vars, function(v) lapply(covars, function(c) kevin$sendRequest(combinedHeatmap, list(x = c, y = v, cohorts = params$cohorts))))
    heatmaps <- lapply(vars, function(v){
      lapply(covars, function(c){
        x <- qCommand(reqQ, rPath, message = list(fun = 'combinedHeatmap',
                                             args = list(req$cookies[['sid']], x = c, y = v, show = 'all', cohorts = params$datasets), timeout = 500))
        jsonlite::fromJSON(x$message,simplifyDataFrame = FALSE, simplifyMatrix = TRUE)
      })
   })

      #res$set_body(jsonlite::toJSON(list(quants = sapply(quants, '[[', 'message'), heatmaps = lapply(heatmaps, function(x) lapply(x, '[[', 'message'))), auto_unbox = TRUE))
     # res$set_body(jsonlite::toJSON(list(quants = sapply(quants, '[[', 'message')), auto_unbox = TRUE))
    res$set_body(jsonlite::toJSON(list(quants = quants, heatmaps= heatmaps)))

  }

)

x <- list()
x <- lapply(1:10, function(i){
a  <- matrix(rnorm(3*i), ncol=3)

c <- as.big.matrix(a)
 describe(c)
})
