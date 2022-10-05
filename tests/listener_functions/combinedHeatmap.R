combinedHeatmap <- function (sid, x = NULL, y = NULL,  show = "zoomed",
                             numints = 20, method = "smallCellsRule", k = 3, noise = 0.25,
                             cohorts = NULL) {

  for (var in c(x,y)){
    if(varmap[[var]]$type != 'number'){
      # nothing to do here:
      return(NULL)
    }
  }
  if(!is.null(cohorts)){
    cohorts <- strsplit(cohorts, ',\\s*')
  }

  finalCohorts <- Reduce(function(x,y){ # keep only the common datasources
    if(is.null(x)){
      return(y)
    } else if(is.null(y)){
      return(x)
    } else {
      return(intersect(x,y))
    }
  }, list(cohorts, varmap[[x]]$cohorts, varmap[[y]]$cohorts))


  type = 'combine'
  if (is.null(finalCohorts)) {
    datasources <- datashield.connections_find()
  } else {
    datasources <- opals[finalCohorts]
  }

  ############## important for all functions!!!! #################
  dfName <- restrictToRelevantCols(var, sid, 'working_set', datasources)
 # on.exit(
#    try(datashield.rm(datasources, dfName)),
 #   add = TRUE, after = FALSE
#  )

  ##################################################################


  if (is.null(x)) {
    stop("x=NULL. Please provide the names of the 1st numeric vector!",
         call. = FALSE)
  }
  if (is.null(y)) {
    stop("y=NULL. Please provide the names of the 2nd numeric vector!",
         call. = FALSE)
  }
  x <- paste0('working_set$',x)
  y <- paste0('working_set$',y)

  if (method != "smallCellsRule" & method != "deterministic" &
      method != "probabilistic") {
    stop("Function argument \"method\" has to be either \"smallCellsRule\" or \"deterministic\" or \"probabilistic\"",
         call. = FALSE)
  }
  xnames <- extract(x)
  x.lab <- sub('working_set$', '', xnames[[length(xnames)]], fixed = TRUE)
  ynames <- extract(y)
  y.lab <- sub('working_set$', '',  ynames[[length(ynames)]], fixed = TRUE)
  stdnames <- names(datasources)
  num.sources <- length(datasources)
  if (method == "deterministic") {
    method.indicator <- 1
    cally <- paste0("heatmapPlotDS(", x, ",", y, ",", k,
                    ",", noise, ",", method.indicator, ")")
    anonymous.data <- DSI::datashield.aggregate(datasources,
                                                cally)
    pooled.points.x <- c()
    pooled.points.y <- c()
    for (i in 1:num.sources) {
      pooled.points.x[[i]] <- anonymous.data[[i]][[1]]
      pooled.points.y[[i]] <- anonymous.data[[i]][[2]]
    }
  }
  if (method == "probabilistic") {
    method.indicator <- 2
    cally <- paste0("heatmapPlotDS(", x, ",", y, ",", k,
                    ",", noise, ",", method.indicator, ")")
    anonymous.data <- DSI::datashield.aggregate(datasources,
                                                cally)
    pooled.points.x <- c()
    pooled.points.y <- c()
    for (i in 1:num.sources) {
      pooled.points.x[[i]] <- anonymous.data[[i]][[1]]
      pooled.points.y[[i]] <- anonymous.data[[i]][[2]]
    }
  }

  if (method == "smallCellsRule") {
    cally <- paste("rangeDS(", x, ")")
    x.ranges <- DSI::datashield.aggregate(datasources,
                                          as.symbol(cally))
    cally <- paste("rangeDS(", y, ")")
    y.ranges <- DSI::datashield.aggregate(datasources,
                                          as.symbol(cally))
    x.minrs <- c()
    x.maxrs <- c()
    y.minrs <- c()
    y.maxrs <- c()
    for (i in 1:num.sources) {
      x.minrs <- append(x.minrs, x.ranges[[i]][1])
      x.maxrs <- append(x.maxrs, x.ranges[[i]][2])
      y.minrs <- append(y.minrs, y.ranges[[i]][1])
      y.maxrs <- append(y.maxrs, y.ranges[[i]][2])
    }
    x.range.arg <- c(min(x.minrs), max(x.maxrs))
    y.range.arg <- c(min(y.minrs), max(y.maxrs))
    x.global.min <- x.range.arg[1]
    x.global.max <- x.range.arg[2]
    y.global.min <- y.range.arg[1]
    y.global.max <- y.range.arg[2]
    cally <- paste0("densityGridDS(", x, ",", y, ",",
                    limits = T, ",", x.global.min, ",", x.global.max,
                    ",", y.global.min, ",", y.global.max, ",", numints,
                    ")")
    grid.density.obj <- DSI::datashield.aggregate(datasources,
                                                  as.symbol(cally))
    numcol <- dim(grid.density.obj[[1]])[2]
    for (i in 1:num.sources) {
      message(stdnames[i], ": ", names(dimnames(grid.density.obj[[i]])[2]))
    }
    Global.grid.density <- matrix(0, dim(grid.density.obj[[1]])[1],
                                  numcol - 2)
    for (i in 1:num.sources) {
      Global.grid.density <- Global.grid.density +
        grid.density.obj[[i]][, 1:(numcol - 2)]
    }
  }
  else {
    if (method == "deterministic" | method == "probabilistic") {
      xvect <- unlist(pooled.points.x)
      yvect <- unlist(pooled.points.y)
      y.min <- min(yvect)
      x.min <- min(xvect)
      y.max <- max(yvect)
      x.max <- max(xvect)
      y.range <- y.max - y.min
      x.range <- x.max - x.min
      y.interval <- y.range/numints
      x.interval <- x.range/numints
      y.cuts <- seq(from = y.min, to = y.max, by = y.interval)
      y.mids <- seq(from = (y.min + y.interval/2),
                    to = (y.max - y.interval/2), by = y.interval)
      y.cuts[numints + 1] <- y.cuts[numints + 1] *
        1.001
      x.cuts <- seq(from = x.min, to = x.max, by = x.interval)
      x.mids <- seq(from = (x.min + x.interval/2),
                    to = (x.max - x.interval/2), by = x.interval)
      x.cuts[numints + 1] <- x.cuts[numints + 1] *
        1.001
      grid.density <- matrix(0, nrow = numints, ncol = numints)
      for (j in 1:numints) {
        for (k in 1:numints) {
          grid.density[j, k] <- sum(1 * (yvect >= y.cuts[k] &
                                           yvect < y.cuts[k + 1] & xvect >= x.cuts[j] &
                                           xvect < x.cuts[j + 1]), na.rm = TRUE)
        }
      }
      grid.density.obj <- list()
      grid.density.obj[[1]] <- cbind(grid.density,
                                     x.mids, y.mids)
      numcol <- dim(grid.density.obj[[1]])[2]
      Global.grid.density <- grid.density
    }
  }
  graphics::par(mfrow = c(1, 1))
  x <- grid.density.obj[[1]][, (numcol - 1)]
  y <- grid.density.obj[[1]][, (numcol)]
  z <- Global.grid.density
  if (show == "all") {
    png('./heatmap.png')
    fields::image.plot(x, y, z, xlab = x.lab, ylab = y.lab,
                       main = "Heatmap Plot of the Pooled Data")
    dev.off()
    x <- x  # to keep the code format
  }
  else if (show == "zoomed") {
    flag <- 0
    rows_top <- 1
    while (flag != 1) {
      if (all(Global.grid.density[rows_top, ] == 0)) {
        rows_top <- rows_top + 1
      }
      else {
        flag <- 1
      }
    }
    if (rows_top == 1) {
      dummy_top <- rows_top
    }
    else {
      dummy_top <- rows_top - 1
    }
    flag <- 0
    rows_bot <- dim(Global.grid.density)[1]
    while (flag != 1) {
      if (all(Global.grid.density[rows_bot, ] == 0)) {
        rows_bot <- rows_bot - 1
      }
      else {
        flag <- 1
      }
    }
    if (rows_bot == dim(Global.grid.density)[1]) {
      dummy_bot <- rows_bot
    }
    else {
      dummy_bot <- rows_bot + 1
    }
    flag <- 0
    col_left <- 1
    while (flag != 1) {
      if (all(Global.grid.density[, col_left] == 0)) {
        col_left <- col_left + 1
      }
      else {
        flag <- 1
      }
    }
    if (col_left == 1) {
      dummy_left <- col_left
    }
    else {
      dummy_left <- col_left - 1
    }
    flag <- 0
    col_right <- dim(Global.grid.density)[2]
    while (flag != 1) {
      if (all(Global.grid.density[, col_right] == 0)) {
        col_right <- col_right - 1
      }
      else {
        flag <- 1
      }
    }
    if (col_right == 1) {
      dummy_right <- dim(Global.grid.density)[2]
    }
    else {
      dummy_right <- col_right + 1
    }
    z.zoomed <- Global.grid.density[dummy_top:dummy_bot,
                                    dummy_left:dummy_right]
    x.zoomed <- x[dummy_top:dummy_bot]
    y.zoomed <- y[dummy_left:dummy_right]
    x <- x.zoomed
    y <- y.zoomed
  }
  else {
    stop("Function argument \"show\" has to be either \"all\" or \"zoomed\"")
  }
  return(list(Global.grid.density, xlab = x.lab, ylab = y.lab, x = x, y = y))
}
