makeRoxygenTemplate <- function(funfile) {
  fun_text <- readLines(funfile, warn=FALSE)
  
  if (grepl("?#'", fun_text[1])) {
    stop("It appears you alread have roxygen documentation in your function!")
  }

  params <- strsplit(fun_text, ",\\s*|.+?function\\s*\\(|\\s*\\)|\\s*\\{")[[1]]
  params <- params[lapply(params, nchar) > 0]
  params <- gsub("^\\s+|\\s+$|=.+", "", params)
  params <- paste0("#' @param  ", params, " <parameter description goes here>")
  top <- "#' <brief description of function>\n#'\n#' <full description of function>\n#'\n#' @import <list required packages separated by a spaces>"
  end <- "#' @export\n#' @return\n#' @examples \\dontrun{\n#'\n#'}"
  roxy <- paste(c(top, params, end), sep="")
  writeLines(c(roxy, fun_text), funfile)
}