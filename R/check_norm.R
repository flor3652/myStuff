#' @title Check Normality
#' 
#' @description
#' This function is designed to allow users to check the normality of residuals through an intuitive approach. It generates a random set of normally distributed variables, then inserts the given residuals into this set. If users can distinguish the QQ Plot of their data from the randomly generated normal data, their data does not follow a normal distribution.
#' 
#' @param resid Residuals from a fit model
#' @param grid Dimension for the grid of plots, given as an integer.
#' 
#' @details
#' 
#' @author Michael Floren
check_norm <- function(resid, grid=3){
  def_par <- par(mfrow=c(grid, grid), ask=FALSE) #create grid^2 plots: grid^2-1 will be truly normal, and one will be our data... If you can spot the one with our data, then it is different enough to be noticeable (from normal)
  loc_of_real_data <- sample(1:grid^2, size = 1)
  
  distractors <- list()
  for(i in 1:grid^2){
    if(i!=loc_of_real_data){
      distractors[[i]] <- rnorm(length(resid), mean=mean(resid), sd=sd(resid)) #saving these for the "reveal" plot
      qqnorm(distractors[[i]])
    } else if(i==loc_of_real_data)
      qqnorm(resid)
  }
  
  readline(prompt = "Hit enter when ready to see your data...")
  par(mfrow=c(grid,grid))
  for(i in 1:grid^2){
    if(i!=loc_of_real_data){
      qqnorm(distractors[[i]])
    } else if(i==loc_of_real_data)
      qqnorm(resid, col="red")
  }
  par(mfrow=def_par)
}