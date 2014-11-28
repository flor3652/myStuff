#' @title The Keep Function
#' 
#' @description
#' This function is designed to keep a given list of variables (as opposed to the rm function, which is designed to remove a list of variables)
#' 
#' @param x A character vector of variables which you wish to keep
#' 
#' @details
#' The function uses the \code{ls} command to find the variables that are defined in the global environment. It then deletes all variables that are not given as an argument. It does not delete itself
#' 
#' 


# note that in order to read the environment of all of the variables, we need to say "ls(name=.GlobalEnv)" (as just saying "ls()" would just give the variables that are defined in the function environment)

keep <- function(x = c()){
  if(!is.character(x)) stop("Need character input")
  rm(list=ls(name=.GlobalEnv)[ls(name=.GlobalEnv)!="keep" & !ls(name=.GlobalEnv)%in%x], pos=.GlobalEnv)
}




#### Testing ----
# works 
for(i in 1:26) assign(letters[i],i)
ls()
keep(c("a","b"))
ls()

# numerics throw an error (they would "work", but it wouldn't keep anything)
keep(1)
ls()

# takes single character input (if you just want to keep the one item)
for(i in 1:26) assign(letters[i],i)
ls()
keep("a")
ls()























