downscaleCV.keras.tfprobability <- function(x, 
                                            y, 
                                            cnn_model.args,
                                            type_nn,
                                            samples = 1,
                                            sampling.strategy = "kfold.chronological", 
                                            folds = 4, 
                                            scaleGrid.args = NULL,
                                            prepareData.keras.args = NULL,
                                            compile.args = NULL,
                                            fit.args = NULL) {
  
  x <- getTemporalIntersection(x,y,which.return = "obs")
  y <- getTemporalIntersection(x,y,which.return = "prd")
  
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
    prepareData.keras.args[["x"]] <- xT
    prepareData.keras.args[["y"]] <- yT
    xy <- do.call("prepareData.keras",args = prepareData.keras.args) 
    model_full <- do.call("cnn_model",args = cnn_model.args) 
    model <- if (type_nn == "multivariate-gaussian") {
      model_full
    } else if (type_nn == "cvae" || type_nn == "cvae-fwi" || type_nn == "vae" || type_nn == "vae-fwi") {
      model_full$model
    }
    input <- if (type_nn == "multivariate-gaussian" || type_nn == "cvae" || type_nn == "vae") {
      xy$x.global
    } else if (type_nn == "cvae" || type_nn == "cvae-fwi" || type_nn == "vae-fwi") {
      dim(xy$x.global) <- c(dim(xy$x.global)[1],prod(dim(xy$x.global)[2:4]))
      cbind(xy$x.global, xy$y$Data)
    }
    # weights_embedding_1 <- model_full$embedding$weights[[1]][1,1,1,1]
    # weights_encoder_1 <- model_full$encoder$weights[[1]][1,1]
    # weights_decoder_1 <- model_full$decoder$weights[[1]][1,1]
    model$compile(loss = compile.args[["loss"]], 
                  optimizer = compile.args[["optimizer"]])
    model$fit(input, 
              xy$y$Data, 
              "batch_size" = fit.args[["batch_size"]], 
              "epochs" = fit.args[["epochs"]], 
              "validation_split" = fit.args[["validation_split"]],
              "verbose" = fit.args[["verbose"]], 
              "callbacks" = fit.args[["callbacks"]])
    ###
    # weights_embedding_2 <- model_full$embedding$weights[[1]][1,1,1,1]
    # weights_encoder_2 <- model_full$encoder$weights[[1]][1,1]
    # weights_decoder_2 <- model_full$decoder$weights[[1]][1,1]
    xt <- prepareNewData.keras(newdata = xt, data.structure = xy)
    lapply(1:samples, FUN = function(sample) {
      print(sprintf("sample: %s", sample))
      if (type_nn == "multivariate-gaussian") {
        p_dist <- model(xt$x.global$member_1)
        p <- p_dist %>% tfd_sample() %>% as.array() 
      } else if (type_nn == "cvae" || type_nn == "cvae-fwi") {
        input_test <- if (type_nn == "cvae") {
          xt$x.global$member_1
        } else if (type_nn == "cvae-fwi") {
          dim(xt$x.global$member_1) <- c(dim(xt$x.global$member_1)[1],prod(dim(xt$x.global$member_1)[2:4]))
          cbind(xt$x.global$member_1, xy$y$Data[1:nrow(xt$x.global$member_1),]) 
        }
        xt_embedded <- model_full$embedding$predict(input_test)
        latent_dim <- model_full$decoder$input$shape[[2]] - ncol(xt_embedded)
        xt_sample <- mvrnorm(n = nrow(xt_embedded), mu = rep(0, latent_dim), Sigma = diag(1, nrow = latent_dim, ncol = latent_dim))
        input_to_decoder <- cbind(xt_embedded,xt_sample)
        p <- model_full$decoder$predict(input_to_decoder)
      } else if (type_nn == "vae-fwi" || type_nn == "vae") {
        latent_dim <- cnn_model.args[["latent_dim"]]
        input_to_decoder <- mvrnorm(n = dim(xt$x.global$member_1)[1], mu = rep(0, latent_dim), Sigma = diag(1, nrow = latent_dim, ncol = latent_dim))
        p <- model_full$decoder$predict(input_to_decoder)
      }
      p[p < 0] <- 0
      ## prediction matrix to climate4r object... -------
      out <- yt
      out$Data <- p 
      attr(out$Data, "dimensions") <- c("time", "loc")
      ###
      k_clear_session()
      rm(model, model_full)
      gc()
      return(out)
    }) %>% bindGrid(dimension = "member")
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
