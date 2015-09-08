#' @title The Keep Function
#' 
#' @description
#' This function is designed to keep a given list of variables (as opposed to the rm function, which is designed to remove a list of variables)
#' 
#' @param ... Variables that should stay in the environment
#' @param x A character vector of variables which you wish to keep
#' 
#' @details
#' The function uses the \code{ls} command to find the variables that are defined in the global environment. It then deletes all variables that are not given as an argument.
#' 
#' @author Michael Floren
#' 
#  For whatever reason, I CANNOT get this link to the "rm" function to actually create a hyperlink
#' @seealso \code{\link[base]{rm}}
#' 


# note that in order to read the environment of all of the variables, we need to say "ls(name=.GlobalEnv)" (as just saying "ls()" would just give the variables that are defined in the function environment)

keep <- function(...,x=c())
{
  
  if(length(x))
  {
    if(!is.character(x)) stop("x must contain character vector")
    rm(list=ls(name=.GlobalEnv)[!ls(name=.GlobalEnv)%in%x], pos=.GlobalEnv)
    return(invisible(ls(name=.GlobalEnv)))
  }
  
  dots <- match.call(expand.dots=FALSE)$...
  
  if (length(dots) && !all(sapply(dots, function(x) is.symbol(x) || 
                                    is.character(x)))) 
    stop("... must contain names or character strings")
  
  names <- sapply(dots,as.character)
  
  rm(list=ls(name=.GlobalEnv)[!ls(name=.GlobalEnv) %in% names], pos=.GlobalEnv)
  
  invisible(ls(name=.GlobalEnv))
}
