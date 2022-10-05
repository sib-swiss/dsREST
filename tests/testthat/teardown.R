if(stopThem)
  lapply(listeners, function(x){
    qCommand(reqQ, resPath, title = 'STOP')
  })
  Sys.sleep(1)
  system2('docker', args = conts) # conts comes from setup

