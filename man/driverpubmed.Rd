% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/driverpubmed.R
\name{driverpubmed}
\alias{driverpubmed}
\title{driverpubmed}
\usage{
driverpubmed(
  term = "Melanoma",
  term_AND_1 = "TCGA",
  term_AND_2 = "Signature",
  term_AND_3 = NULL,
  term_AND_4 = NULL,
  term_AND_5 = NULL,
  term_AND_6 = NULL,
  term_AND_7 = NULL,
  term_AND_8 = NULL,
  term_AND_9 = NULL,
  year_start = "2021",
  year_end = "2024"
)
}
\arguments{
\item{term}{you want to search what}

\item{term_AND_1}{you want to search what else and}

\item{year_start}{you know bro}

\item{year_end}{you know bro}
}
\value{
html and text
}
\description{
publications find
}
\examples{
# examples
driverpubmed(term = "Melanoma",
             term_AND_1 = "TCGA",
             term_AND_2 = "Signature",
             term_AND_3 = NULL,
             term_AND_4 = NULL,
             year_start = "2024",
             year_end = "2024")
}
