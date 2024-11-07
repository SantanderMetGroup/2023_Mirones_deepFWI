downscale.convLSTM <-  function(x, 
                                y, 
                                model.args,
                                sampling.strategy = "kfold.chronological", 
                                folds = 4, 
                                time_steps = 0,
                                scaleGrid.args = NULL,
                                compile.args = NULL,
                                fit.args = NULL) {
  
  x <- getTemporalIntersection(x,y,which.return = "obs")
  y <- getTemporalIntersection(x,y,which.return = "prd")
  
  x.aux <- x$Data  # Dimensions:(removing member dimension): (var, time, lat, lon)
  y.aux <- y$Data  # Dimensions: (time,loc)

  #reorder dimensions: (time_steps, lat, lon, vars)
  x.aux <- aperm(x.aux, c(2, 3, 4, 1))
  #create sequences with time steps

  num_samples <- dim(x.aux)[1] - time_steps + 1
  x_seq <- array(0, dim = c(num_samples, time_steps, dim(x.aux)[2], dim(x.aux)[3], dim(x.aux)[4]))
  y_seq <- array(0, dim = c(num_samples, dim(y.aux)[2]))
    
  for (i in 1:num_samples) {
    x_seq[i, , , , ] <- x.aux[i:(i + time_steps - 1), , , ]
    y_seq[i, ] <- y.aux[i + time_steps - 1, ]  # Predict last step of the sequence
  }
    
  x <- subsetDimension(x, dimension = "time", indices = time_steps:getShape(x, "time"))
  y <- subsetDimension(y, dimension = "time", indices = time_steps:getShape(y, "time"))
  
  x$Data <- x_seq
  y$Data <- y_seq
  
  attr(x$Data, "dimensions") <- c("time", "member", "lat", "lon", "var") #add time_steps as member dimension
  attr(y$Data, "dimensions") <- c("time", "loc")
  
  if (!is.null(sampling.strategy)) {
    if (sampling.strategy == "leave-one-year-out") {
      type <- "chronological"
      folds <- as.list(getYearsAsINDEX(y) %>% unique())
    }
    
    if (sampling.strategy == "kfold.chronological") {
      type <- "chronological"
      if (!is.numeric(folds)) {
        folds.user <- unlist(folds) %>% unique() %>% sort()
        folds.data <- getYearsAsINDEX(y) %>% unique()
        if (any(folds.user != folds.data)) stop("In the parameters folds you have indicated years that do not belong to the dataset. Please revise the setup of this parameter.")
      }
    }
    if (sampling.strategy == "kfold.random") {
      type <- "random"
      if (!is.numeric(folds)) stop("In kfold.random, the parameter folds represent the NUMBER of folds and thus, it should be a NUMERIC value.")
    }
  }
  if (is.list(folds)) {
    if (any(duplicated(unlist(folds)))) stop("Years can not appear in more than one fold")
  }
  
  data <- dataSplit(x,y, f = folds, type = type)
  p <- lapply(1:length(data), FUN = function(xx) {
    message(paste("fold:",xx,"-->","calculating..."))
    xT <- data[[xx]]$train$x ; yT <- data[[xx]]$train$y
    xt <- data[[xx]]$test$x  ; yt <- data[[xx]]$test$y
    yT <- filterNA(yT)
    xT <- intersectGrid(xT,yT,which.return = 1)
    if (!is.null(scaleGrid.args)) {
      scaleGrid.args$base <- xT
      scaleGrid.args$grid <- xt
      scaleGrid.args$skip.season.check <- TRUE
      xt <- do.call("scaleGrid",args = scaleGrid.args)
      scaleGrid.args$grid <- xT
      xT <- do.call("scaleGrid",args = scaleGrid.args)
    }
    

    dims.order <- c(which(getDim(xT) == "time"),
                    which(getDim(xT) == "member"),
                    which(getDim(xT) == "lat"),
                    which(getDim(xT) == "lon"),
                    which(getDim(xT) == "var"))
    
    xT$Data <- aperm(xT$Data, dims.order) 
    attr(xT$Data, "dimensions") <- c("time","member","lat","lon","var")
    xt$Data <- aperm(xt$Data, dims.order) 
    attr(xt$Data, "dimensions") <- c("time","member","lat","lon","var")
    
    model <- do.call("convLSTM_model",args = model.args) 
    
    model$compile(loss = compile.args[["loss"]], 
                  optimizer = compile.args[["optimizer"]])
    model$fit(xT$Data, 
              yT$Data, 
              "batch_size" = fit.args[["batch_size"]], 
              "epochs" = fit.args[["epochs"]], 
              "validation_split" = fit.args[["validation_split"]],
              "verbose" = fit.args[["verbose"]], 
              "callbacks" = fit.args[["callbacks"]])
    
    p_dist <- model(xt$Data)
    p <- p_dist %>% tfd_sample() %>% as.array() 
    p[p < 0] <- 0
    ## prediction matrix to climate4r object... -------
    out <- yt
    out$Data <- p 
    attr(out$Data, "dimensions") <- c("time", "loc")
    ####
    k_clear_session()
    rm(model)
    gc()
    return(out)
    
  }) %>% bindGrid(dimension = "time")
  if (!isRegular(y)) {
    if (getShape(y,"loc") == 1) {
      p <- redim(p, member = FALSE, loc = TRUE)
    } else {
      p <- redim(p, member = FALSE, drop = TRUE, loc = TRUE)
    }
  }
  return(p)
}

###example of use:


# downscale.convLSTM(x = x, y = y,
#                    model.args = list(input_shape = c(7, getShape(x, "lat"), getShape(x, "lon"), getShape(x, "var")),
#                                      output_shape = getShape(y, "loc"),
#                                      kernel_size = c(3,3),
#                                      neurons = c(200, 200)),
#                    sampling.strategy = "kfold.chronological", 
#                    folds = 4, 
#                    time_steps = 7,
#                    compile.args = list("loss" = negloglik, "optimizer" = optimizer_adam(lr = 0.0001)),
#                    fit.args = list("batch_size" = 100L, "epochs" = 10000L, "validation_split" = 0.1, "verbose" = 0L, "callbacks" = list(callback_early_stopping(patience = 30))),
#                    scaleGrid.args = list(type = "standardize"),
#                    folds = folds)
