dms_dd <- function(x, sep=":", hem) {
  ## Convert degrees minutes seconds to decimal degrees:
  # x: a vector containing the lat or long with elements separated by single character
  # sep: the character separating the degrees, minutes, seconds (default ":")
  # hem: the hemisphere ("N","S","E","W").  Assumes all coords in the same hemisphere
  if (hem %in% c("N","S","E","W")) {
    x <- lapply(strsplit(x,sep), as.numeric)
    x <- unlist(lapply(x, function(y) (y[1]+y[2]/60+y[3]/3600)))
    ifelse(hem %in% c("N","E"),
           ifelse(x>0,mult <- 1, mult <- -1),
           ifelse(x<0,mult <- 1, mult <- -1))
    x <- x*mult
    x
  } else {
    print("Error: 'hem' must be N,S,E, or W")
  }
}

###############################################################################

utm_dd <- function(zones,easting,northing) {
  ## Convert zone+utm pairs to lat/long
  #
  # Depends: rgdal
  #
  # zones = numeric (if all coordinates are in the same zone, or vector of 
  #         zones the same length as coordinate pairs
  # easting = numeric or vector of eastings
  # nortings = numeric or vector of northings
  #
  # Returns: a dataframe with five columns: zones, easting, northing, 
  #          Longitude, and Latitude
  
  d <- data.frame(zones,easting,northing,Longitude=NA,Latitude=NA)
  require(rgdal)
  for (zone in unique(zones)) {
    utm <- SpatialPoints(d[d$zones==zone,c(2,3)], 
                         proj4string=CRS(paste("+proj=utm +zone="
                                               , zone, sep="")))
    sp <- spTransform(utm, CRS("+proj=longlat"))  
    d[d$zones==zone,c(4,5)] <- coordinates(sp)                     
  }
  d
}
