makeRoxygenTemplate <- function(funfile) {
  ## Fill in the boilerplate roxygen template at the top of the file containing 
  ## the function.
  ##
  ## funfile: path to the .R file containing the function
  ## 
  ## File must have the argument list and the opening "{" all on one line: 
  ##    myFun <- function(arg1, arg2, arg3) {
  ## 
  ## Inspired by Karthik Ram's RTools Sublime Text 2 plugin:
  ## https://github.com/karthik/Rtools
  
  fun_text <- readLines(funfile, warn=FALSE)
  
  if (grepl("^#'", fun_text[1])) {
    stop("It appears you already have roxygen documentation in your function!")
  }
  
  # Find the function and parameter definition line:
  matches <- regexpr("(?<=\\().+?(?=\\)\\s*?\\{)", fun_text, perl=TRUE)
  params_line <- regmatches(fun_text,matches)[1]
  
  # Parse out and clean the parameter names:
  params <- strsplit(params_line, ",")[[1]]
  params <- gsub("^\\s+|\\s+$|=.+", "", params)
  
  # Put together the roxygen fields:
  params <- paste0("#' @param  ", params, " <parameter description goes here>")
  top <- "#' <brief description of function>\n#'\n#' <full description of function>\n#'\n#' @import <list required packages separated by a spaces>"
  end <- "#' @export\n#' @return\n#' @examples \\dontrun{\n#'\n#'}"
  roxy <- paste(c(top, params, end), sep="")
  
  # Write to the top of the file (without asking... should be safe, i think)
  writeLines(c(roxy, fun_text), funfile)
  
  # Open the file to fill in documentation
  file.edit(funfile)
}