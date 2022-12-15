library(jsonlite)
library(magrittr)
options(datashield.progress = FALSE)

on.exit(unlink('.pq', recursive = TRUE))
stopThem <<- TRUE
x <- system2('docker', args = c('ps'), stdout = TRUE )
if(length(grep('docker_nodes_dsrest',x, ignore.case = TRUE)) < 6){
  system2('docker-compose', args = c('-f', '../docker_nodes_dsREST/docker-compose.yml', 'up', '-d'))
  Sys.sleep(60)
  x <- system2('docker', args = c('ps'), stdout = TRUE )
  stopThem <<- TRUE
}

if(stopThem){
  conts <<- lapply(x, function(y){
    out <- strsplit(y, '\\s+')[[1]]
    out[length(out)]
  }) %>% unlist
  conts[1] <- 'stop' # conts now contains the args for a 'docker stop' command - will be executed in teardown
}


setP <- function(p, pth){
  unlink(pth, recursive = TRUE)
  txtq(pth)$push(p, NULL)
}

###### read the configuration:
#confFile <- '../config.json
confFile <- '/mnt/shareddisk/mip/datashield-engine/conf/config.json'
config <- readChar(confFile, file.info(confFile)$size) %>%
  gsub('(?<=:)\\s+|(?<=\\{)\\s+|(?<=,)\\s+|(?<=\\[)\\s+','',., perl = TRUE) %>%
  fromJSON()
assign('config', config, envir = .GlobalEnv)


############ only for testing #################
#config$sessionTimeout <- 30 # enough for tests
write(jsonlite::toJSON(config), '../configtest.json')
confFile <- '../configtest.json'
#############################################

assign('conts', conts, envir = .GlobalEnv)

#### set the password:

setP('3xC@libur', '.pq')

################ launch the listener(s)


genPath <- paste0(tempdir(TRUE), '/', config$dir)
if(!file.exists(genPath)){
  dir.create(genPath)
}
reqPath <- paste0(genPath, '/requests')
resPath <- paste0(genPath, '/responses')
reqQ <- txtq(reqPath)
assign('reqQ', reqQ, envir = .GlobalEnv)

listeners <- startListeners(reqPath, resPath, config$workers, confFile)

##very important:
unlink('.pq', recursive = TRUE)

assign('listeners', listeners, envir = .GlobalEnv)

################# get the varmap:

globalResPath <- paste0(resPath, '/global')
bubbleJSON <-qCommand(reqQ, globalResPath, message = list(fun = 'identity', args = list(quote(bubble))))$message ### %>% fromJSON(simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
 # lckq(globalResPath)$destroy()  # this should be in prod
assign('globalResPath', globalResPath, envir = .GlobalEnv)
assign('bubbleJSON', bubbleJSON, envir = .GlobalEnv)
######


##### function to reload the listener funcs =====

reloadListener <- function(){
  qCommand(reqQ, resPath, message = list(fun = '.sourceFuncs', args = list(config$listenerFuncDir)))
}

#####
deleteme <- function(){
  qCommand(reqQ, globalResPath, message = list(fun = 'datashield.symbols', args = list(quote(opals))))$message %>% fromJSON()
  qCommand(reqQ, globalResPath, message = list(fun = 'ds.table', args = list("working_set$race", datasources = quote(opals['sophia.db']))))
  qCommand(reqQ, globalResPath, message = list(fun = 'datashield.aggregate', args = list(quote(opals), quote(quote(selfUpgrade('dsBase' ,NULL, NULL, NULL , NULL, TRUE))))))
  qCommand(reqQ, globalResPath, title = 'STOP')
  qCommand(reqQ, globalResPath, message = list(fun = 'get', args = list('sliceNdice')))
  qCommand(reqQ, globalResPath, message = list(fun = 'ds.summary', args = list("tmp_816444147297c7db7ce4e560573c2e33" , datasources = quote(opals))))
  qCommand(reqQ, globalResPath, message = list(fun = 'datashield.aggregate', args = list(quote(opals), "selfUpgrade('dsQueryLibraryServer' ,NULL, NULL, NULL , NULL, TRUE)")))

  }

## app stuff:

sentry <- makeSentryFunction(requestQ = reqQ, responsePath = resPath, loginFuncName = 'authLogin')

sbck<- SentryBackend$new( FUN = sentry)

sentryMw <- AuthMiddleware$new(
  auth_backend = sbck,
  routes = "/",
  match = "partial",
  id = "sentry_middleware"
)

app <- loadApp(sentryMw)

