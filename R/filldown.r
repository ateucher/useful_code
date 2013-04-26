filldown <- function(x) {
  notna <- !is.na(x) # elements with values     
  ix <- cumsum(notna) # index to previous element (but zeros where we need NA)     
  ix[ix==0] <- NA # use [NA] as index to produce NA in output     
  return(x[notna][ix]) # for each: return previous value if found, else NA 
}