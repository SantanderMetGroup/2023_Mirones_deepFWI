corr_length <-function(Din,tr=0.5)
{
  #Description:
  # The correlation length is calculated based on the smoothed fit to the given data.The maximum correlation is calculated as the first intersection
  # of the smoothed distance vs correlation line with the horizontal line at the given threshold level (tr) to the right of the correlation maximum
  #Input: 
  #  Din:   Data structure as produces by corrMat.VALUE. On the highest level, it is a list with one nested list per season.
  #  tr:  A given corrlation threshold for with the correlation length has to be calculated
  
  #Values: 
  #Structure with one nested list per season that contains the following fields 
  #  corrL: the correlation length
  #  tag: a quality tag saying whether the threshold has been met within the existing data range (i), 
  #       is beyond the lower end of the distance range ('l'), or beyond the higher 
  #       end of the distance range ('h').
  #  tagmax: a quality tag saying whether the maximum correlation occured at the shortest available distance (y) or not (n)
  #  x:   Distances between the data points
  #  y:   correlation length between data pairs
  #  x_fit: Distances used for the correlation length estimation
  #  y_fit: Smoothed curve of correlations used for the correlation length estimation
  #  lat: Latitudes of all stations
  #  lon: Longitudes of all stations
  
  # Thomas Bosshard, April 2016
  
  #seasons present in the input list
  seasons=names(Din)
  #Output should be a list
  values=list()
  
  #Loop over the seasons
  for (si in 1:length(seasons))
  {
    #First, reorder the correlation data into a list having the field "dist" and "MI"
    
    D=list(dist=c(), corrL=c())
    
    lat=attr(Din[[si]],'lat')
    lon=attr(Din[[si]],'lon')
    
    ns=length(lat) #Number of stations
    sseq=seq(1,(ns-1),1)
    
    #Go through all possible pairs of stations
    for (i in sseq)
    {
      eseq=seq((i+1),ns,1)
      
      for (j in eseq)
      {
        
        
        #total distance
        D$dist=rbind(D$dist,distVincentySphere(cbind(lat[i],lon[i]),cbind(lat[j],lon[j]) , r=6378137)/1000)
        #Add MI to output object 
        D$corrL=rbind(D$corrL,Din[[si]][i,j])
        
        
        
      }
    }
    #Smooth the data
    corr_Dist<-smooth_corr(D)
    
    message("Find correlation length for given threshold")
    #Initialize sublist in output object
    values[[seasons[si]]] <- list(corrL=c(), tag=list(),tagmax=list(), x_fit=c(), y_fit=c(), x=c(), y=c(), lat=c(), lon=c())
    
    corrmax=max(corr_Dist$y, na.rm = TRUE)
    corrmin=min(corr_Dist$y, na.rm = TRUE)
    
    corrmaxind=which(corr_Dist$y==corrmax,arr.ind=TRUE)
    if (corrmaxind==1)
    {
      values[[seasons[si]]]$tagmax='y'
      #All correlation values are used for correlation length estimation
      
      corrvalues <- corr_Dist$y
      
    } else
    {
      values[[seasons[si]]]$tagmax='n'
      #Correlation values to the left of corrmaxind are excluded from correlation length estimation
      
      corrvalues <- corr_Dist$y
      corrvalues[1:(corrmaxind-1)]=NA
    }
    
    
    trind <- which(corrvalues<tr, arr.ind=TRUE)
    
    
    
    if (tr <= corrmax & tr >= corrmin)
    {
      values[[seasons[si]]]$corrL <- corr_Dist$x[trind[1]-1]
      values[[seasons[si]]]$tag <- 'i'
      
    } else
    {
      #Find out on which side of the data range the treshold value would be expected (assuming that correlations are increasing and decreasing to the left and right end, respectively)
      
      
      if (tr > corrmax)
      {
        values[[seasons[si]]]$tag <- 'l'
        values[[seasons[si]]]$corrL <- NA
        
      }else
      {
        
        values[[seasons[si]]]$tag <- 'h'
        values[[seasons[si]]]$corrL <- NA
      }
    }
    
    values[[seasons[si]]]$x_fit <- corr_Dist$x
    values[[seasons[si]]]$y_fit <- corr_Dist$y
    values[[seasons[si]]]$x <- D$dist
    values[[seasons[si]]]$y <- D$corr
    values[[seasons[si]]]$lat <- lat
    values[[seasons[si]]]$lon <- lon
  }
  
  return(values)
  
}