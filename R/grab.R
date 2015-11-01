#' @title The Grab Function
#' 
#' @description
#' This function is designed to be used in place of library and install.packages. It will check if a potential package is downloaded and, if not, it will try to download it using install.packages.
#' 
#' @param ... Packages that should be loaded
#' @param list A character list of packages that should be loaded
#' @param dependencies Passed to \code{install.packages}. Default is \code{TRUE}
#' 
#' @details
#' The function uses the logical output from \code{require} to determine if the package exists locally. If it does, the package is then loaded using \code{library}. If not, the function tries do download the package using \code{install.packages}, checks to make sure it downloaded, then loads the package.
#' 
#' @author Michael Floren
#' 
#' @seealso \code{\link[base]{install.packages}, \code{\link[base]{require}}, \code{\link[base]{library}}}
#' 
 
#looks good for a first draft...

grab <- function(..., list = character(), dependencies=TRUE){
  #straight from the rm function
  dots <- match.call(expand.dots = FALSE)$...
  if (length(dots) && !all(vapply(dots, function(x) is.symbol(x) || 
                                  is.character(x), NA, USE.NAMES = FALSE))) 
    stop("... must contain names or character strings")
  names <- vapply(dots, as.character, "")
  if (length(names) == 0L) 
    names <- character()
  pkgs <- c(names,list)
  if(length(pkgs)==0)
    stop("No input: what do you want me to do?")
  
  for(i in 1:length(pkgs)){
    #if the packages isn't local, try to download it; else load the package
    if(!suppressWarnings(require(pkgs[i], character.only=TRUE, quietly=TRUE))) {
      #install the missing package
      install.packages(pkgs[i], dependencies=dependencies)
      
      #if installed, load the package; else kick an error
      if(suppressWarnings(require(pkgs[i], character.only=TRUE, quietly=TRUE))) {
        library(pkgs[i], character.only=TRUE)
      } else stop("The ", pkgs[i]," package cannot be found or installed.")
      
    } else library(pkgs[i], character.only=TRUE)
    
  }
  
}
