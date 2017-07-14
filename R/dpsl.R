#' @title Dput Select.list Shortcut
#' 
#' @description
#' This function is a shortcut to use dput select.list for variable names from a data frame.
#' 
#' @param df The data frame that the list should come from
#' @param ind The index of names that should be used: 1/'row" for rownames, 2/'col' for column names
#' @param multiple Passed to select.list: should multiple entries be used? Defaults to TRUE.
#' @param graphics Passed to select.list: should graphics be used? Defaults to TRUE.
#' @param quotes Do you want quotes around each element? Defaults to TRUE.
#' @param ... Passed to select.list
#' 
#' @details
#' The function simply rewrites some defaults for select.list and wraps it in a dput. It has an option to select columns or rows, simply by changing the input from "columnames" to "rownames" via an if statement. Nothing fancy, but certainly much shorter than what I normally use.
#' 
#' @examples
#' #to list selected column names from iris (control click to select multiple)
#' dpsl(iris)
#' 
#' #to list selected rows, use ind="row"
#' dpsl(iris, ind="row") #could also use dpsl(iris, ind=1
#' 
#' #to select columns from iris
#' iris[,dpsl(iris)]
#' 
#' @author Michael Floren
#' 
#' @seealso \code{\link[base]{dput}, \code{\link[utils]{select.list}}}
#' 

#looks good for a first draft...

dpsl <- function(df, ind = "col", multiple =TRUE, graphics=TRUE, quotes=TRUE, ...){
  if(quotes){ #if you want quotes
    tf <- function(x) dput(select.list(x, multiple=multiple, graphics=graphics, ...))
  } else if (!quotes) {
    tf <- function(x) print(paste0("c(",paste(select.list(x, multiple=multiple, graphics=graphics, ...), collapse=", "), ")"))
  }
  
  if(ind==2 | ind=="col"){
    out <- tf(colnames(df))
  } else if (ind == 1 | ind == "row"){
    out <- tf(rownames(df))
  } else if(ind == 3 | ind == "raw"){
    out <- tf(df)
  } else{
    stop("Index value is invalid. Please give 1, 2, 'row', or 'col'")
  }
}
