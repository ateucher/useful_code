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

utm_dd <- function(utms) {

  ## Convert zone+utm pairs to lat/long.  Note, NOT vectorized!
  #
  # Depends: rgdal
  #
  # utms: a list of the form c(zone,easting,northing) OR
  #       a one-row dataframe with 3 columns in the order zone, easting, northing
  #
  # Returns: a dataframe with five columns: zones, easting, northing, 
  #          Longitude, and Latitude
  
  require(rgdal)

  if (class(utms) == "numeric") {
    d <- data.frame(t(utms),Longitude=NA,Latitude=NA)
  } else {
    d <- data.frame(utms,Longitude=NA,Latitude=NA)
  }

  names(d)[1:3] <- c("Zone", "Easting", "Northing")

  utm <- SpatialPoints(d[2:3], proj4string=CRS(paste("+proj=utm +zone="
                                               , d[1], sep="")))
  sp <- spTransform(utm, CRS("+proj=longlat"))  
  d[4:5] <- coordinates(sp)                     
  d
}
