# #' @title The Keep Function
# #' 
# #' @description
# #' This function is designed to keep a given list of variables (as opposed to the rm function, which is designed to remove a list of variables)
# #' 
# #' @param x A character vector of variables which you wish to keep
# #' 
# #' @details
# #' The function uses the \code{ls} command to find the variables that are defined in the global environment. It then deletes all variables that are not given as an argument. It does not delete itself
# #' 
# #' 
# 
# 
# # note that in order to read the environment of all of the variables, we need to say "ls(name=.GlobalEnv)" (as just saying "ls()" would just give the variables that are defined in the function environment)
# 
# keep <- function(x = c()){
#   if(!is.character(x)) stop("Need character input")
#   rm(list=ls(name=.GlobalEnv)[ls(name=.GlobalEnv)!="keep" & !ls(name=.GlobalEnv)%in%x], pos=.GlobalEnv)
# }
# 
# 
# 
# 
# #### Testing ----
# # # works 
# # for(i in 1:26) assign(letters[i],i)
# # ls()
# # keep(c("a","b"))
# # ls()
# # 
# # # numerics throw an error (they would "work", but it wouldn't keep anything)
# # keep(1)
# # ls()
# # 
# # # takes single character input (if you just want to keep the one item)
# # for(i in 1:26) assign(letters[i],i)
# # ls()
# # keep("a")
# # ls()
# 
# 

#### Improving the keep function ----

#Idea: use the ... idea from the rm function to make the "keep" function work better

#this is the rm function
function (..., list = character(), pos = -1, envir = as.environment(pos), 
          inherits = FALSE) 
{
  dots <- match.call(expand.dots = FALSE)$...
  if (length(dots) && !all(sapply(dots, function(x) is.symbol(x) || 
                                    is.character(x)))) 
    stop("... must contain names or character strings")
  names <- sapply(dots, as.character)
  if (length(names) == 0L) 
    names <- character()
  list <- .Primitive("c")(list, names)
  .Internal(remove(list, envir, inherits))
}


keep <- function(x = c()){
  if(!is.character(x)) stop("Need character input")
  rm(list=ls(name=.GlobalEnv)[ls(name=.GlobalEnv)!="keep" & !ls(name=.GlobalEnv)%in%x], pos=.GlobalEnv)
}




#using a generic function to test out the pieces of the rm function

# 2.01 ---- 
#function will output even if x is not given. It will not throw an error, but as we are specifically printing "x" it will give a null value
blah <- function(...,x=c())
{
  print(match.call(expand.dots=FALSE)$...)
  dots <- match.call()$...
  
  
  print(match.call()$x)
}

blah(y,b,a,x=c(1,2,3))
blah(y,a,b)
q <- list("a","b","c")
blah(q)

ls() %in% q
q[[1]] <- "q"
ls() %in% q


#2.02 ----
# Not printing the match.call()$x (it has no need). Keep x=c() in case we want list to be provided. For some reason this.
blah <- function(...,x=c())
{
  print(match.call(expand.dots=FALSE)$...)
  dots <- match.call()$...
  print(ls(name=.GobalEnv) %in% dots)
}

blah(a,h)

#name=.GlobalEnv DOES exist...
ls(name=.GlobalEnv)
list(a,h)
list("a","h")
q<-list("a","h")
#This works (when the list is a string...). So maybe we need to make the ...s a string?
ls(name=.GlobalEnv) %in% q


#2.03 ----
# There appears to be a couple things in the rm function: a check that the inputs are symbols/characters, and using an "sapply" to change the "dots" to characters for removal.
blah <- function(...,x=c())
{
  print(match.call(expand.dots=FALSE)$...)
  dots <- match.call()$...
  
  #This piece of the function checks the length of "dots" as a logical vector (in essence asking: does it have a length?). Both this and the next piece "should" be true, otherwise we want the function to stop and display the error message. The next piece should be looked at from the inside out. First, the "sapply" piece is checking if all of the elements in "dots" are symbols or characters (returning a vector of TRUE/FALSE for each element). The "all" statment will return TRUE if all of the elements in that vector (created by sapply) are TRUE. Finally, if all of there elements return true, we don't have a problem. We want when all of the elements DON'T return true to throw an error. Thus if either "length(dots)" is FALSE (dots doesn't have a length), or if not all of the pieces of dots are characters/symbols, the error message will be output.
  if (length(dots) && !all(sapply(dots, function(x) is.symbol(x) || 
                                    is.character(x)))) 
    stop("... must contain names or character strings")
  
  print(ls(name=.GobalEnv) %in% dots)
}

blah(a,h)


#trying out stuff from the rm function to see how it works, then putting it in the above
a
is.symbol(a)
is.character(a)
rm(1)
length(q)
if(length(q))
print(1)
as.logical(length(q))
?all
sapply(q, function(x) is.symbol(x) || is.character(x))
sapply(q, function(x) is.symbol(x) & is.character(x))
sapply(q, function(x) is.symbol(x) | is.character(x))
sapply(q, function(x) is.symbol(x) || is.character(x))
all(sapply(q, function(x) is.symbol(x) || is.character(x)))
as.logical(length(x))
x <- NULL
as.logical(length(x))

























