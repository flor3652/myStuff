#' 
#' @title Fix P-Values
#' 
#' @description 
#' A function to fix p-values in summaries for xtable. P-values should never display zero (mathematically), so this attempts to correct them. This will mostly be used for the coefficient section of summary tables from R when going into x-table
#' 
#' @param x The summary table to "fix" the p-values for
#' @param dig The number of digits you wish to round to
#' 
#' 
#' @details Thus function expects summary tables (and turns them into a data.frame). It assumes that the last column of the table contains p-values (with the name Pr..., normal in many summary tables). It will shoot a warning letting you know if you are using the function with a table that doesn't have a name that starts with this for the last column, though it will still run. The function takes the data frame and manipulates all values that round to 0 (at the set digits) to instead display that the p-value is less than the current rounded digits outcome. Note that this makes the last column of the output dataframe either character or numeric.
#' 
#' @examples 
#' #this gives a summary table with a small p-value
#' (mod <- coef(summary(lm(uptake ~ conc + Treatment + Type + Plant, data=CO2))))
#' 
#' #this fixes the p-value to 4 digits, correctly reporting p-values that would have been rounded to 0
#' fixp(mod,dig=4)
#' 
#' @author Michael Floren

fixp <- function(x, dig=2)
  {
    x <- as.data.frame(x)
    if (substr(names(x)[ncol(x)], 1, 2) != "Pr") 
      warning("The name of the last column didn't start with Pr. This may indicate that p-values weren't in the last row, and thus, that this function is inappropriate.")
    x[, ncol(x)] <- round(x[, ncol(x)], dig)
    for (i in 1:nrow(x)) {
      if(is.na(x[i,ncol(x)])){
        x[i, ncol(x)] <- NA
      } else if (x[i, ncol(x)] == 0) 
        x[i, ncol(x)] <- paste0("< .", paste0(rep(0, dig - 1), collapse = ""), "1")
    }
    x
  }
