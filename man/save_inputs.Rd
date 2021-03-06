% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exported-tidy-helpers.R
\name{save_inputs}
\alias{save_inputs}
\title{Save data to model inputs}
\usage{
save_inputs(data, filename, subdir = NULL)
}
\arguments{
\item{data}{A data.frame.}

\item{filename}{Name to use for saved file. Must include a file extension that is
either a delim or feather file.}

\item{subdir}{An optional subdir.}
}
\description{
\code{save_inputs()} utilizes the power of a tidy project structure to save a data.frame
to the model inputs folder.
}
