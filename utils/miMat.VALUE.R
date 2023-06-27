miMat.VALUE <- function(stationObj,
                        predictionObj = NULL,
                        season = c("annual", "DJF", "MAM", "JJA", "SON"),
                        aggr.type = c("after","before"),
                        prob = NULL, 
                        threshold = 1,
                        max.na.prop = 0.25,
                        wt = NULL) {
  season <- match.arg(season, choices = c("annual", "DJF", "MAM", "JJA", "SON","JJAS"), several.ok = TRUE)
  aggr.type <- match.arg(aggr.type, choices = c("after", "before"))
  prob.types <- c("DD", "DW", "WW", "WD")
  if (!is.null(prob)) {
    if (length(prob) > 1) stop("Invalid 'prob' definition")
    if (prob <= 0 || prob >= 1) stop("Invalid 'prob' definition")
  }
  mi.list <- lapply(1:length(prob.types), function(t) {
    prob.type <- prob.types[t]
    message("**** ", prob.type, " ****")
    ineq1 <- substr(prob.type, 1, 1)
    ineq2 <- substr(prob.type, 2, 2)
    ineqs <- sapply(c(ineq1, ineq2), function(x) switch(x, "D" = "<", "W" = ">="))
    if (is.null(prob)) {
      expr.PrA <- paste0("sum(A", ineqs[1], "threshold, na.rm = TRUE)/length(A)")
      expr.PrB <- paste0("sum(B", ineqs[1], "threshold, na.rm = TRUE)/length(B)")
      expr.AgivenB <- paste0("A[which(B", ineqs[2], "threshold)]")
      expr.PrAgivenB <- paste0("sum(AgivenB", ineqs[1], "threshold,na.rm=TRUE)/length(AgivenB)")
    } else {## For a given threshold exceedance
      expr.PrA <- paste0("sum(A",ineqs[1],"quantile(A,probs = prob,na.rm=TRUE),na.rm=TRUE)/length(A)")
      expr.PrB <- paste0("sum(B",ineqs[1],"quantile(B,probs = prob,na.rm=TRUE),na.rm=TRUE)/length(B)")
      expr.AgivenB <- paste0("A[which(B", ineqs[2], "quantile(B, probs = prob, na.rm=TRUE))]")
      expr.PrAgivenB <- paste0("sum(AgivenB", ineqs[1], "quantile(A,probs=prob,na.rm=TRUE),na.rm=TRUE)/length(AgivenB)")
    }
    o <- stationObj
    stationObj <- NULL
    if (!is.null(predictionObj)) {
      o <- suppressWarnings(dimFix(predictionObj))
    }
    mat.list <- lapply(1:length(season),  function(x) {
      sea <- switch(season[x], 
                    "annual" = 1:12,
                    "DJF" = c(12,1,2),
                    "MAM" = 3:5,
                    "JJA" = 6:8,
                    "SON" = 9:11,
                    "JJAS"= 6:9)
      if(!is.null(wt)){
        o <- na.filter.VALUE(dimFix(subsetDimension(o, dimension = "time", indices = wt)), max.na.prop)
      }else{
        o <- na.filter.VALUE(dimFix(subsetGrid(o, season = sea)), max.na.prop)
      }
      
      n.mem <- dim(o$Data)[1]
      # Member aggregation before
      if (aggr.type == "before") {
        if (n.mem > 1) message("[", Sys.time(), "] - Aggregating members before computing joint probabilities...")
        o$Data <- apply(o$Data, MARGIN = c(2,3), FUN = mean, na.rm = TRUE)
        attr(o$Data, "dimensions") <- c("time", "station")
        o <- dimFix(o)
        if (n.mem > 1) message("[", Sys.time(), "] - Done.")
        n.mem <- dim(o$Data)[1]
      }
      n.stations <- dim(o$Data)[3]
      mat <- o$Data
      o$Data <- NULL
      message("[", Sys.time(), "] - Calculating probabilities for ", season[x], "...")
      jp.list <- lapply(1:n.mem, function(i) {
        jpmat <- matrix(nrow = n.stations, ncol = n.stations)
        for (j in 1:n.stations) {
          na.a <- which(is.na(mat[i,,j]))
          ind.diff <- setdiff(1:n.stations, j)
          for (k in ind.diff) {
            # Filter the common missing data in A and B:
            na.b <- which(is.na(mat[i,,k]))
            na.ind <- union(na.a, na.b)      
            if (length(na.ind) > 0) {
              A <- mat[i,-na.ind,j]
              B <- mat[i,-na.ind,k]
            } else {
              A <- mat[i,,j]
              B <- mat[i,,k]
            }
            # Marginal probabilities
            PrA <- eval(parse(text = expr.PrA))
            PrB <- eval(parse(text = expr.PrB))
            # Conditional probability Pr(A|B)
            AgivenB <- eval(parse(text = expr.AgivenB))
            PrAgivenB <- eval(parse(text = expr.PrAgivenB))                        
            # Joint probability P(A,B) = P(B,A) = P(A)*P(B|A) P(B)*P(A|B)
            PrAB <- PrB*PrAgivenB
            # Mutual information 
            jpmat[j,k] <- PrAB*log((PrAB/(PrA*PrB))) # P(A,B)*log[P(A,B)/P(A)*P(B)]
          }
        }
        jpmat[which(jpmat < 0)] <- 0 # small negatives may appear due to rounding errors
        return(jpmat)
      })
      arr <- do.call("abind", c(jp.list, along = -1L))
      jp.list <- NULL
      # Member aggregation "after"
      if (aggr.type == "after") message("[", Sys.time(), "] - Aggregating members...")
      mimat <- unname(apply(arr, MARGIN = c(2,3), FUN = mean, na.rm = TRUE))
      arr <- NULL
      attr(mimat, "station_names") <- o$Metadata$name
      attr(mimat, "lon") <- unname(o$xyCoords[,1])
      attr(mimat, "lat") <- unname(o$xyCoords[,2])
      return(mimat)
    })
    names(mat.list) <- season
    return(mat.list)
  })
  names(mi.list) <- prob.types
  out.list <- lapply(1:length(mi.list[[1]]), FUN = function(x) {
    mi.list[[1]][[x]] + mi.list[[2]][[x]] + mi.list[[3]][[x]] + mi.list[[4]][[x]]
  })
  names(out.list) <- season
  mi.list <- NULL
  attr(out.list, "threshold_exceedance") <- prob
  message("[", Sys.time(), "] - Finished.")
  return(out.list)
}