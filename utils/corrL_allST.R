corrL_allST <- function(data,predictionObj=NULL,type,corrtype,tr,season)
{
  
  D_all=corrMat.VALUE(data,predictionObj=predictionObj,type=type, season=season,method=corrtype,deseason=FALSE,max.na.prop = 1)
  
  corrL_All <- corr_length(D_all,tr=tr)
  
  return(corrL_All)
}