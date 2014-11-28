#' @title Set global parameters
#' 
#' @param N The population size
#' @param n The sample size
#' @param alpha The level of significance (Type 1 error rate)
#' @param d The acceptable margin of error in terms of the variable of interest (for instance, 100 trees off). This is used in sample size calculations.
#' 
#' @description 
#' \code{set_param} takes input and sets global parameters
#' 
#' @return
#' This function will return all inputs, in addition to defining \code{tcut} and \code{zcut}. These are vectors, defined from the input \code{alpha} and \code{n}, that give the positive and negative cutoff values respective to their name
#' 
#' @examples
#' set_param(N=100,n=20,alpha=.05)
#' 
#' @details
#' This function sets global parameters that are used in many statistical analysis, including calculating z and t cutoff scores. If no information is given, only the alpha and zcutoff will be set.







set_param <- function(N=NA, n=NA, alpha=.05, d=NA){
  if(!is.na(N)) {
    N <<- N
    cat("N = ",N,"\n",sep='')
  } else cat("N not set\n")
  if(!is.na(n)) {
    n <<- n
    cat("n = ",n,"\n",sep='')
  } else cat("n not set\n")
  alpha <<- alpha
  cat("alpha = ",alpha,"\n",sep='')
  if(!is.na(n)) {
    tcut <<- qt(c(alpha/2,1-alpha/2),n-1,...)
    cat("t-cutoffs (tcut) = ",tcut[1],", ",tcut[2],"\n",sep='')
  } else cat("t-cutoffs (tcut) not set\n")
  zcut <<- qnorm(c(alpha/2,1-alpha/2),...)
  cat("z-cutoffs (zcut) = ",zcut[1],", ",zcut[2],"\n",sep='')
  if(!is.na(d)) {
    d <<- d
    cat("allowable difference (d) for sample size estimations = ",d,"\n",sep='')
  } else cat("d not set\n")
}

