#' @title The Keep Function
#' 
#' @param x A character vector of variables which you wish to keep
#' 
#' @details
#' This function is designed to keep a given list of variables (as opposed to the rm function, which is designed to remove a list of variables)

# note that in order to read the environment of all of the variables, we need to say "ls(name=.GlobalEnv)" (as just saying "ls()" would just give the variables that are defined in the function environment)

keep <- function(x = c()){
  rm(list=ls(name=.GlobalEnv)[ls(name=.GlobalEnv)!="keep" & !ls(name=.GlobalEnv)%in%x], pos=.GlobalEnv)
}