ascii_export <- function(pred, filename, stations, dest.dir){
  
  ref.stations <- read.csv(stations)
  ref <- as.character(ref.stations$station_id)
  message("[",Sys.time(),"] Writing ", filename, " file")
  
  dates <- getRefDates(pred) 
  refdates <- paste0(substr(dates,1,4),
                     substr(dates,6,7),
                     substr(dates,9,10))
  
  ind <- match(ref, pred$Metadata$station_id)
  df <- cbind.data.frame(refdates, pred$Data[,ind])
  names(df) <- append("YYYYMMDD", ref)
  write.csv(df, file = paste0(dest.dir,
                              filename,
                              ".txt"),
            quote = FALSE,
            row.names = FALSE)
  
}