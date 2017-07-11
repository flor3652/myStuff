#' 
#' @title Eval Parse Text
#' 
#' @description 
#' This is a short function which implements eval(parse(text=__)) quickly (including a minor error check (that the blank is text)).
#' 
#' @param x This is the text to parse
#' 
#' 
#' @details Not much to it! Currently requires a character.
#' 
#' @examples 
#' #this gives 1
#' a=1
#' ept("a")
#' 
#' @author Michael Floren
#' 
ept <- function(x){
  if(mode(x)!="character")
    stop("Need a character entry")
  
  eval(parse(text=x))
}
