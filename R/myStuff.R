# This file is here just for notation purposes (in case anyone tries to find information on the package using "?myStuff")
# Wasn't working earlier. The two lines immediately below is trying to mimic something done by Hadley Wickham. And it works.
#' 
#' @name myStuff
#' @docType package
#' 
#' 
#' 
#' @title My Stuff
#' 
#' @description
#' This is a package that contains some useful functions that were originally created for me and my personal needs.
#' 
#' @param set_param This function can be used to set global statistical parameters from all in one place. When given N, n, and alpha, it will set those as global parameters as well as calculating z-scores and t-scores for the given population.
#' @param keep The keep function was designed to work in much the same was as rm, save to keep objects as opposed to removing them.
#' @param grab The grab function is designed to be a face version of library, simply attempting to download packages that aren't already downloaded.
#' 
#' @details
#' As a side note, the \code{keep} function was designed out of several projects I had where I moved quickly between them. I wanted to clear the workspace to avoid naming confusion, but I also wanted several variables to be left alone. \code{keep} can remove all variables from the workspace other than the ones listed

#note that this code may not actually be usable as overall documentation of the functions.
NULL