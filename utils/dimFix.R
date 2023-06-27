dimFix <- function(valueObj) {
  # Add fake 'loc' dimension to single-station datasets
  # add condition that station dim doesn't exists, because if it is TRUE station dim is duplicated
  if ((!("loc" %in% attr(valueObj$Data, "dimensions"))) & (!("station" %in% attr(valueObj$Data, "dimensions")))) {
    dimNames <- c(attr(valueObj$Data, "dimensions"), "station")
    perm <- if (length(attr(valueObj$Data, "dimensions")) == 2) { # "member","time"
      c(2,3,1)
    } else {# "time"
      c(2,1)
    }
    valueObj$Data <- unname(aperm(abind(valueObj$Data, NULL, along = 0), perm = perm))
    attr(valueObj$Data, "dimensions") <- dimNames
  }
  # Add fake member dimension to deterministic/obs
  if (!("member" %in% attr(valueObj$Data, "dimensions"))) {
    dimNames <- c("member", attr(valueObj$Data, "dimensions"))
    valueObj$Data <- unname(abind(valueObj$Data, NULL, along = -1))    
    attr(valueObj$Data, "dimensions") <- dimNames
  }
  
  if (("loc" %in% attributes(valueObj$Data)$dimensions)) {
    
    idx <- which(attributes(valueObj$Data)$dimensions == "loc")
    attributes(valueObj$Data)$dimensions[idx] <- "station"
    
  }
  
  return(valueObj)
}