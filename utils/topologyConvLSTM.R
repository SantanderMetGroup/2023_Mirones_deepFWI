convLSTM_model <- function(input_shape, output_shape, kernel_size, neurons){
  
  model <- keras_model_sequential() %>%
    layer_conv_lstm_2d(
      filters = 50,
      kernel_size = kernel_size,  #kernel size
      activation = "relu",
      padding = "valid",
      return_sequences = TRUE,  #return the complete sequences
      input_shape = input_shape  # (time_steps, lat, lon, var)
    ) %>%
    layer_conv_lstm_2d(
      filters = 25,
      kernel_size = kernel_size,
      activation = "relu",
      padding = "valid",
      return_sequences = TRUE
    ) %>%
    layer_conv_lstm_2d(
      filters = 10,
      kernel_size = kernel_size,
      activation = "relu",
      padding = "valid",
      return_sequences = FALSE  #last layer do not return the complete sequences
    ) %>%
    layer_flatten() %>%
    layer_dense(units = neurons[1], activation = "relu") %>%
    layer_dense(units = neurons[2], activation = "relu") %>%
    layer_dense(units = params_size_multivariate_normal_tri_l(event_size = output_shape)) %>%
    layer_multivariate_normal_tri_l(event_size = output_shape)
  
  return(model)
}

# use:
# convLSTM_model(input_shape = getShape(x)[2:5], output_shape = getShape(y)[2], 
#                kernel_size = c(3,3), neurons = c(200,200))