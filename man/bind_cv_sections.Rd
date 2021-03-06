% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/template_functions.R
\name{bind_cv_sections}
\alias{bind_cv_sections}
\title{Bind Rmd files}
\usage{
bind_cv_sections(sections = list.files(".", ".Rmd"),
  name = paste(Sys.Date(), "_CV.Rmd", sep = ""),
  yaml = system.file("rmarkdown/templates/cv/skeleton/skeleton.Rmd", package =
  "cvR"), replace = FALSE)
}
\arguments{
\item{sections}{List containing the names and rutes (as character) of
files to bind. Default to \code{list.files(".", ".Rmd")}.}

\item{name}{Name and rute of the returned file. Default to
\code{paste(Sys.Date(), '_CV.Rmd', sep = '')}}

\item{yaml}{Name and rute to file containing yaml preamble. Default to
\code{skeleton.Rmd} in the package files}

\item{replace}{If FALSE, function return a warning if output document
already exists, asking for a change in the name of the output. If TRUE,
no check is done and output file is overwritten}
}
\value{
A file resulting of the bind of the selected Rmd files
}
\description{
Function to bind rmd files containing CV sections
}

