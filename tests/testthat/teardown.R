if(stopThem)
  lapply(listeners, function(x){
    try(qCommand(reqQ, globalResPath, title = 'STOP', timeout = 10))
  })
  Sys.sleep(1)
  system2('docker', args = conts) # conts comes from setup


