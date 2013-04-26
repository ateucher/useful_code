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