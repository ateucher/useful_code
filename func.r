google_ss <- function(key = NA, gid=0) {
  # This doesn't allow querying the spreadsheet, but seems to handle
  # mixed datatype columns better than getGoogleSS
  if (is.na(key)) {stop("\nDocumentkey (key) is missing\n")}
  require(RCurl)
  url <- getURL(paste("https://docs.google.com/spreadsheet/pub?key=", key,
                      "&single=true&gid=", gid, "&output=csv", sep = ""),
                cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
  read.csv(textConnection(url), header = T, sep = ",")
}

getGoogleSS <- function(ssKey,query="",gid=0) {
  # ssKey: the long spreadsheet identifier
  # query: SQL query string
  # gid: the sheet gid (gid), default is the first sheet (0)
  #
  require(RCurl)
  tt <- getForm("https://spreadsheets.google.com/tq" 
                , key = ssKey, tqx='out:csv'
                , tq= curlEscape(query), gid = gid,
                .opts = list(followlocation = TRUE, verbose = TRUE
                            , ssl.verifypeer = FALSE))
  return(read.csv(textConnection(tt),stringsAsFactors=F))
}

###############################################################################

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


colClasses <- function(d, colClasses) {
  # Coerces data.frame columns to the specified classes
  # Example usage
  # DF <- as.data.frame(matrix(rnorm(25), 5, 5))
  # DF2 <- colClasses(DF, c(rep("character", 3), rep("factor", 2)))
  #
  # DF3 <- colClasses(DF, 'Date')
  # str(DF3)
  colClasses <- rep(colClasses, len=length(d))
  d[] <- lapply(seq_along(d)
                , function(i) switch(colClasses[i], 
                                     numeric=as.numeric(d[[i]]), 
                                     character=as.character(d[[i]]), 
                                     Date=as.Date(d[[i]]
                                                  , origin='1970-01-01'), 
                                     POSIXct=as.POSIXct(d[[i]]
                                                        , origin='1970-01-01'), 
                                     factor=as.factor(d[[i]]),
                                     as(d[[i]], colClasses[i]) ))
  d
}


###############################################################################

plotShapeCodes <- function() {
  ## plot shape codes for plotting in R:
  ## Run with no parameters plotShapeCodes()
  library(ggplot2)
  myTitle <- "Guide to Point Shape codes in R
     (from http://www.win-vector.com/blog/2012/04/how-to-remember-point-shape-codes-in-r/)
  \n"
  sum <- ggplot() + ggtitle(myTitle) + theme(plot.title=element_text(hjust=0))
  for(i in 1:25) {
    sum <- sum + geom_point(data=data.frame(x=c(i))
                            , aes(x=x,y=x), shape=i
                            , size=4) + facet_wrap(~x,scales='free')
  }
  sum
}

################################################################################

## Colour-blind friendly palettes
# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
