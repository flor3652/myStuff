#' px Version 0.1
#'
#' @title Print X-Table
#' 
#' @description This function is intended to be a wrapper for printing xtables with some common options that I normally use and change. This provides no unique or creative commands, and is simply used for convenience.
#' 
#' @param x The table to be written to LaTeX
#' @param cap The caption of the table
#' @param lab The label to be assigned
#' @param rn Logical. Do you wish to print row names?
#' @param cn Logical. Do you wish to print column names?
#' @param scale A scaling factor (smaller scale = smaller text)
#' @param ... Additional arguments (passed to xtable and print.xtable)
#' 
#' @details Again, just combining print.xtable into one short function with some convenient defaults for my use.
#' 
#' @author Michael Floren
#' 
#' @seealso \code{\link[xtable]{print.xtable}}
#'
#' 
#' 

px <- function(x, cap=NULL, lab=NULL, rn=FALSE, cn=TRUE, scale="1", ...){ 
  if(!is.null(lab))
    if(substr(lab,1,4) != "tab:") #for compatability
      lab <- paste0("tab:", lab) #auto add "tab:" (like knitr does)
  print(xtable::xtable(x, caption=cap, label=lab, ...), include.rownames=rn, include.colnames=cn, scalebox=scale, ...)
}
