corrMat.VALUE <- function(stationObj,
                          predictionObj = NULL,
                          season = c("annual", "DJF", "MAM", "JJA", "SON","JJAS"),
                          method = "pearson",
                          type = "after",
                          max.na.prop = 0.25,
                          deseason = TRUE,
                          window.width = 31) {
  season <- match.arg(season, choices = c("annual", "DJF", "MAM", "JJA", "SON","JJAS"), several.ok = TRUE)
  method <- match.arg(method, choices = c("pearson", "kendall", "spearman"))
  type <- match.arg(type, choices = c("after", "before"))
  o <- dimFix(stationObj)
  stationObj <- NULL
  if (!is.null(predictionObj)) {
    ## message("[", Sys.time(), "] - Loading predictions...")
    o <- suppressWarnings(dimFix(predictionObj))
    ## message("[", Sys.time(), "] - Done.")            
  }
  n.mem <- dim(o$Data)[1]
  # Member aggregation before
  if (type == "before") {
    if (n.mem > 1) message("[", Sys.time(), "] - Aggregating members before computing correlation...")
    o$Data <- apply(o$Data, MARGIN = c(2,3), FUN = mean, na.rm = TRUE)
    attr(o$Data, "dimensions") <- c("time", "station")
    o <- dimFix(o)
    
    if (n.mem > 1) message("[", Sys.time(), "] - Done.")
    n.mem <- dim(o$Data)[1]
  }
  # Removing seasonal cycle
  if (deseason) o <- deseason.VALUE(o, window.width, max.na.prop)
  mat.list <- lapply(1:length(season),  function(i) {
    sea <- switch(season[i], 
                  "annual" = 1:12,
                  "DJF" = c(12,1,2),
                  "MAM" = 3:5,
                  "JJA" = 6:8,
                  "SON" = 9:11,
                  "JJAS" = 6:9)
    aux <- na.filter.VALUE(dimFix(subsetGrid(o, season = sea)), max.na.prop) #change subsetValue by subsetGrid
    # Correlation matrices for each member
    message("[", Sys.time(), "] - Computing correlation matrix for ", season[i], " period...")
    cor.mat.list <- lapply(1:n.mem, function(x) cor(aux$Data[x,,], use = "pairwise.complete.obs", method = method))
    aux$Data <- NULL
    arr <- do.call("abind", c(cor.mat.list, along = -1L))
    cor.mat.list <- NULL
    # Member aggregation "after"
    cormat <- unname(apply(arr, MARGIN = c(2,3), FUN = mean, na.rm = TRUE))
    arr <- NULL
    attr(cormat, "station_id") <- aux$Metadata$station_id
    attr(cormat, "station_names") <- aux$Metadata$name
    attr(cormat, "lat") <- unname(aux$xyCoords[,2])
    attr(cormat, "lon") <- unname(aux$xyCoords[,1])
    attr(cormat, "lat") <- unname(aux$xyCoords[,2])
    return(cormat)
  })
  names(mat.list) <- season
  message("[", Sys.time(), "] - Done.")
  attr(mat.list, "correlation.type") <- method
  attr(mat.list, "max.na.prop") <- max.na.prop
  attr(mat.list, "deseason") <- deseason
  if (deseason) attr(mat.list, "deseason:window") <- window.width
  return(mat.list)
}