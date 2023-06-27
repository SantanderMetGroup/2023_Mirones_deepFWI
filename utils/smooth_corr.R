smooth_corr <- function(D)
{
  #Smoothing function using a loess filter with standard settings
  
  #Input:
  #D:   List containing the fields 'corr' (for correlations) and 'dist' (for distances between a point pairs [km])
  #
  #Value:
  #     List containg the fields 'x' (for distances) and 'y' (for smoothed correlations)
  
  #Thomas Bosshard, March 2016
  
  message("Apply loess filter")
  
  #smooth the function
  D <- data.frame(corr=unlist(D$corr), dist=unlist(D$dist))
  smf <- loess(corr ~ dist, D)
  
  #Estimate the distances at which the correlation is just above the correlation thresholds given in tr
  #For this, estimate the smoothed function at densily spaced distances
  
  xestim <- seq(min(D$dist),max(D$dist),length.out=10000)
  ypredict <- predict(smf, xestim)
  
  values <- list(x=xestim, y=ypredict, smf=smf)
  
  return(values)
  
}