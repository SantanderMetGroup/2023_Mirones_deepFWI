## function cnn_model ---------
cnn_model <- function(topology, input_shape, output_shape, kernel_size, neurons, prior = NULL, latent_dim = NULL, weight = NULL) {
  
  # cnn-single-site -------------------------------------------------------
  if (topology == "cnn-single-site-gaussian") {
    inputs <- layer_input(shape = input_shape)
    l1 = layer_conv_2d(inputs,filters = 50, kernel_size = kernel_size, activation = "relu", padding = "valid")
    l2 = layer_conv_2d(l1, filters = 25, kernel_size = kernel_size, activation = "relu", padding = "valid")
    l3 = layer_conv_2d(l2, filters = 10, kernel_size = kernel_size, activation = "relu", padding = "valid") 
    l4 = layer_flatten(l3)
    l5 = layer_dense(l4, units = neurons[1], activation = "relu") 
    l6 = layer_dense(l5, units = neurons[2], activation = "relu") 
    outputs = layer_dense(l6, units = output_shape, activation = "linear") 
    model <- keras_model(inputs = inputs, outputs = outputs) 
    ## -------------------    
    return(model) 
  }
  
  # cnn-multi-site -------------------------------------------------------
  if (topology == "cnn-multi-site") {
    inputs <- layer_input(shape = input_shape)
    l1 = layer_conv_2d(inputs,filters = 50, kernel_size = kernel_size, activation = "relu", padding = "valid")
    l2 = layer_conv_2d(l1, filters = 25, kernel_size = kernel_size, activation = "relu", padding = "valid")
    l3 = layer_conv_2d(l2, filters = 10, kernel_size = kernel_size, activation = "relu", padding = "valid") 
    l4 = layer_flatten(l3)
    l5 = layer_dense(l4, units = neurons[1], activation = "relu") 
    l6 = layer_dense(l5, units = neurons[2], activation = "relu") 
    outputs = layer_dense(l6, units = output_shape, activation = "linear") 
    model <- keras_model(inputs = inputs, outputs = outputs) 
    ## -------------------    
    return(model) 
  }
  
  # cnn-multi-site-gaussian -------------------------------------------------------
  if (topology == "cnn-multi-site-gaussian") {
    inputs <- layer_input(shape = input_shape)
    l1 = layer_conv_2d(inputs,filters = 50, kernel_size = kernel_size, activation = "relu", padding = "valid")
    l2 = layer_conv_2d(l1, filters = 25, kernel_size = kernel_size, activation = "relu", padding = "valid")
    l3 = layer_conv_2d(l2, filters = 10, kernel_size = kernel_size, activation = "relu", padding = "valid") 
    l4 = layer_flatten(l3)
    l5 = layer_dense(l4, units = neurons[1], activation = "relu") 
    l6 = layer_dense(l5, units = neurons[2], activation = "relu") 
    l71 = layer_dense(l6, units = output_shape, activation = "linear") 
    l72 = layer_dense(l6, units = output_shape, activation = "linear") 
    outputs <- layer_concatenate(list(l71,l72))  
    model <- keras_model(inputs = inputs, outputs = outputs) 
    ## -------------------    
    return(model) 
  }

  # cnn-multi-site-multi-gaussian -------------------------------------------------------
  if (topology == "cnn-multi-site-multi-gaussian") {
    model <- keras_model_sequential() %>%
      layer_conv_2d(input_shape = input_shape, filters = 50, kernel_size = kernel_size, activation = "relu", padding = "valid") %>% 
      layer_conv_2d(filters = 25, kernel_size = kernel_size, activation = "relu", padding = "valid") %>% 
      layer_conv_2d(filters = 10, kernel_size = kernel_size, activation = "relu", padding = "valid")  %>% 
      layer_flatten() %>% 
      layer_dense(units = neurons[1], activation = "relu")  %>% 
      layer_dense(units = neurons[2], activation = "relu")  %>% 
      layer_dense(units = params_size_multivariate_normal_tri_l(event_size = output_shape))
    
    modelDist <- keras_model_sequential() %>%
      model() %>% 
      layer_multivariate_normal_tri_l(event_size = output_shape)
    ## -------------------    
    return(list("model" = model, "modelDist" = modelDist)) 
  }
}
