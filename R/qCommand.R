#'@export
qCommand <- function(requestQ, responsePath, title = 'fun', message = list(fun = 'identity', args = list('No function specified'), resPath = NULL), wait = TRUE, timeout = 180, every = 1, maxQueues = 10){
  message$waitForIt <- wait

  ########### no wait no complications ################

  if(!wait){
    message <- jsonlite::serializeJSON(message)
    requestQ$push(title, message)
    return(NULL)
  }

  ############### wait for response ##################
  found <- FALSE
  for (i in 1:maxQueues){
    myPath <- paste0(responsePath, '/',i)
    responseQ <- lckq(myPath)
    if(responseQ$lockFor({
      # it's my queue now, first reset it to avoid confusions,  then send the command
      responseQ$reset()
      message$resPath <- myPath
      message <- jsonlite::serializeJSON(message)
      requestQ$push(title, message)
      # yeah, don't think it will reply straight away, start with a break:
      Sys.sleep(every/2)
      response <- responseQ$blockingRead(timeout, every)
    })){
      found <- TRUE
      break
    }
  }
  if(!found){
    stop(paste0('Too many parallel calls in the same session. Maximum allowed is ', maxQueues, '.'))
  }
  return(response)
}


#'@export
testConcurrentRequests <- function(genPath, sleepTime = 5){
  resPath <- paste0(genPath, '/responses/global')
  reqPath <- paste0(genPath, '/requests')
  reqQ <- txtq(reqPath)
  mesg <- list(fun = 'Sys.sleep', args = list(sleepTime))
  invisible(qCommand(reqQ, resPath, message = mesg, wait = TRUE))

}
