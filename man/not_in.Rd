% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/not_in.R
\name{not_in}
\alias{not_in}
\title{not_in}
\usage{
not_in(x, y)
}
\arguments{
\item{x}{character string}

\item{y}{character string}
}
\value{
TRUE or FALSE
}
\description{
Isn 't it inside
}
\examples{
# examples
str = c("aaa","bBB","CcC","dDd")
if("PDCD1" not_in str){
print("not in")
}
}
