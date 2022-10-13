# modified AuthBackendBasic to allow sessions
# the auth_fun's signature must be (user, password = NULL, sid = NULL) and it must always return the sid if it worked or NULL if it didn't

#' @import RestRserve
#' @export
SentryBackend <- R6::R6Class('SentryBackend',
                             inherit = AuthBackendBasic,
                             public = list(
                               authenticate = function(request, response) {

                                  mySid <- request$cookies[['sid']]

                                  res <- NULL
                                  if (!is.null(mySid)){  # if we have a sid

                                    myUser <- request$cookies[['user']]

                                    res <- private$auth_fun(user = myUser, password = NULL, sid = mySid)
                                  } else { # no sid, this is the login
                                    user_password = private$extract_credentials(request, response)
                                    myUser <- user_password[[1]]

                                    res <- private$auth_fun(myUser, user_password[[2]], sid = NULL)
                                  }
                                  if(res == 'unauthorized'){
                                    raise(self$HTTPError$unauthorized(
                                      body = "401 Invalid Username/Password",
                                      headers = list("WWW-Authenticate" = "Basic"))
                                    )
                                  } else if(grepl('timeout', res)){
                                    raise(self$HTTPError$unauthorized(
                                      body = paste0("401 Session Expired, ", res),
                                      headers = list("WWW-Authenticate" = "Basic"))
                                    )
                                  }
                                  if (!is.null(res)) {  # auth_fun must return a sid
                                    mySid <- res
                                    response$set_cookie('sid', mySid) # do I set it every time?
                                    response$set_cookie('user', myUser) # do I set it every time?
                                    return(TRUE)
                                  } else {
                                    raise(self$HTTPError$unauthorized(
                                        body = "401 Invalid Username/Password",
                                        headers = list("WWW-Authenticate" = "Basic"))
                                      )
                                  }
                                }
                             )
)


# on second thoughts, here's an auth fun generator, must be called in the app with
#  mySentryFunction <- makeSEntryFunction(somequeue, someotherpath, sometimeout)

# closure to create a sentry function with a specified request queue and response path
#'@export
makeSentryFunction <- function(requestQ, responsePath, loginFuncName = 'authLogin', timeout = 1800){

  sentryFunc <-  function(user, password = NULL, sid = NULL ){ # must return a sid

    if(is.null(sid)){ # we must login
      if(is.null(password)){ # don't even
        return('unauthorized')
      }
      sid <- paste0(runif(1), Sys.time()) %>% digest
      newPath <- paste0(responsePath, '/', user, sid) # new pipes in here starting with 1

      # send the login command to the listener(s)
      mesg <- list(fun = loginFuncName, args = list(user, password))
      logged <- qCommand(reqQ, newPath, message = mesg, wait = TRUE, timeout = 60)
      if(jsonlite::fromJSON(logged$message) != 'OK'){
        # the response queue has been created already only for this, it needs destroying:
        unlink(newPath, recursive = TRUE, force = TRUE)
        return('unauthorized')
      }
    } else {  # we have a sid, check if we have the pipes
      myPath <- paste0(responsePath, '/', user, sid)
      if(!file.exists(myPath)){
        return('unauthorized')
      }
#      lastTime <- file.info(myPath)[,'ctime']
#      if(is.na(lastTime) ){ # if it doesn't exist
#        return('unauthorized')
#      }
 #     if(as.numeric(Sys.time()) - as.numeric(lastTime) > timeout){ # too old, sorry
#        unlink(myPath, recursive = TRUE, force = TRUE)
 #       return(paste0('timeout after ', timeout, ' seconds'))
  #    }
    }
    return(sid)
  }

  return(sentryFunc)
}

#'@export
reapOldSessions <- function(rPath, timeout = 1800){

     sesss <- list.dirs(rPath, full.names = TRUE, recursive = FALSE) %>% grep('global', ., invert = TRUE, value = TRUE) # 'global' belongs to the main process

     for(s in sesss){
        parQs <- list.dirs(s, full.names = TRUE, recursive = FALSE) # possibly more than one queue, for requests in parallel in the same session (./1, ./2 etc)
        if(length(parQs) == 0){ # something went wrong, remove the session and go to the next
          unlink(s, recursive = TRUE)
          next
        }
        lastTime <- paste0(parQs, '/head') %>% file.info %>% '[['('mtime') %>% max
         if(as.numeric(Sys.time()) - as.numeric(lastTime) > timeout){ # off with their heads
           # but proceed nicely, go one by one, leave it alone if somebody locks it right now:
         sapply(parQs, function(x){
             q <- lckq(x)
             q$lockFor(q$destroy()) # if it's already locked this returns FALSE without waiting
           }) %>% all %>% 'if'(unlink(s, recursive = TRUE))
         }
     }

}
