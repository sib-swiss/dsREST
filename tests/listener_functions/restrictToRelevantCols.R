
restrictToRelevantCols <- function(cols, sid, df = 'working_set', datasources = NULL){
  if(is.null(datasources)){
    datasources = opals
  }
  tempName <- paste0('tmp_', sid)
  keepcols <- paste(cols, collapse = "','") %>% paste0("c('",., "')") # make it stringy
  compCases <- paste0("complete.cases(", df, "[,", keepcols, "])")
  dssSubset(tempName, df, row.filter = compCases, col.filter = keepcols, datasources = datasources)

  return(tempName)
}
