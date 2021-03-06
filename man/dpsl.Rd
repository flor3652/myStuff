% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dpsl.R
\name{dpsl}
\alias{dpsl}
\title{Dput Select.list Shortcut}
\usage{
dpsl(df, ind = "col", multiple = TRUE, graphics = TRUE, quotes = TRUE, ...)
}
\arguments{
\item{df}{The data frame that the list should come from}

\item{ind}{The index of names that should be used: 1/'row" for rownames, 2/'col' for column names}

\item{multiple}{Passed to select.list: should multiple entries be used? Defaults to TRUE.}

\item{graphics}{Passed to select.list: should graphics be used? Defaults to TRUE.}

\item{quotes}{Do you want quotes around each element? Defaults to TRUE.}

\item{...}{Passed to select.list}
}
\description{
This function is a shortcut to use dput select.list for variable names from a data frame.
}
\details{
The function simply rewrites some defaults for select.list and wraps it in a dput. It has an option to select columns or rows, simply by changing the input from "columnames" to "rownames" via an if statement. Nothing fancy, but certainly much shorter than what I normally use.
}
\examples{
#to list selected column names from iris (control click to select multiple)
dpsl(iris)

#to list selected rows, use ind="row"
dpsl(iris, ind="row") #could also use dpsl(iris, ind=1

#to select columns from iris
iris[,dpsl(iris)]

}
\seealso{
\code{\link[base]{dput}, \code{\link[utils]{select.list}}}
}
\author{
Michael Floren
}
