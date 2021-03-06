% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CampbellCast.R
\name{CampbellCast}
\alias{CampbellCast}
\title{Function to re-organise Campbell data into a format suitable for creating figures per sensor type}
\usage{
CampbellCast(data, use.parallel = FALSE)
}
\arguments{
\item{data}{Data frame with AgFace Campbell logger data that was imported by either \code{\link{CampbellAllImport}}.}

\item{use.parallel}{Logical. Option to enable parallel processing for the file import. Parallel-computing has to be configured before this option can be used. Defaults to FALSE. Unfortunately, it does not speed up the \code{joining} part of the data reorganisation.}
}
\value{
data frame with values of all sensors of a given type in the same column
}
\description{
Merges sensor data from multiple sensors of the same type into one column. Identifies each value using SensorID created from the Sensor number, SYSTEM name and TIMESTAMP.
}

