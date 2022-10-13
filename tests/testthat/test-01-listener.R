test_that("listener listens to basic commands", {

  resQ <- txtq(globalResPath)
  #### the message has this structure:
  # $resPath - path to the response queue
  #### no pid for now, try with one response queue per request # $pid - pid of the requesting process (to differentiate between 2 simultaneous requests from the same user)
  # $fun - the *name* of the function to be called (the function must be defined before)
  # $args - a list containing the function arguments
  # $waitForIt -  a flag indicating a result should be returned in the response queue
  ############
  mesg <- list(fun = 'ls', args = list(quote(.GlobalEnv)) , resPath = globalResPath)
  reqQ$push('fun', jsonlite::serializeJSON(mesg))
  Sys.sleep(1)
  x <- resQ$pop()
  y <- jsonlite::fromJSON(x$message)
  resQ$reset()
  expect_true('opals' %in% y)

})

test_that("qCommand  works", {
 # resPath <- paste0(genPath, '/testdir')
  #### the message has this structure:
  # $resPath - path to the response queue
  #### no pid for now, try with one response queue per request # $pid - pid of the requesting process (to differentiate between 2 simultaneous requests from the same user)
  # $fun - the *name* of the function to be called (the function must be defined before)
  # $args - a list containing the function arguments
  # $waitForIt -  a flag indicating a result should be returned in the response queue
  ############


  code <- paste0("dsREST::testConcurrentRequests('",genPath,"',", 5,")")
  conc  <- processx::process$new('/usr/bin/Rscript',
                                          c('-e',code), cleanup = TRUE, stderr = '', stdout = '')

  mesg <- list(fun = 'names', args = list(quote(opals)))
  Sys.sleep(1)

  nms <- qCommand(reqQ, globalResPath, message = mesg, wait = TRUE, timeout = 10)
  Sys.sleep(7)
  q1 <- txtq(paste0(globalResPath, '/1'))
  q2 <- txtq(paste0(globalResPath, '/2'))
  expect_equal(q1$log()$message,'{}')
  expect_equal(jsonlite::fromJSON(q2$log()$message),c("omop_test.db","test.db","sophia.db"))

})

test_that("authLogin  works", {
  mesg <- list(fun = 'authLogin', args = list(user = 'guest', pass = 'guest123'))
  logged <- qCommand(reqQ, globalResPath, message = mesg, wait = TRUE, timeout = 150)
  expect_equal(logged$message, '"OK"')
  mesg <- list(fun = 'authLogin', args = list(user = 'guest', pass = 'guest1SSS23'))
  logged <- qCommand(reqQ, globalResPath, message = mesg, wait = TRUE, timeout = 150)
  expect_equal(logged$title, 'error')
})

