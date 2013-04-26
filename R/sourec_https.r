source_https <- function(url, ...) {
  # Source a script from a https (e.g., github) server.  From Tony Breyal here:
  # http://tonybreyal.wordpress.com/2011/11/24/source_https-sourcing-an-r-script-from-github/
  # I have this in my Rprofile.site
  
  ## Example
  # source_https("https://raw.github.com/tonybreyal/Blog-Reference-Functions/
  #               master/R/bingSearchXScraper/bingSearchXScraper.R", 
  #              "https://raw.github.com/tonybreyal/Blog-Reference-Functions/
  #               master/R/htmlToText/htmlToText.R")
  
  # load package
  require(RCurl)
  
  # parse and evaluate each .R script
  sapply(c(url, ...), function(u) {
    eval(parse(text = getURL(u, followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))), envir = .GlobalEnv)
  })
}