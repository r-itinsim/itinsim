% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/request.r
\name{handle_request_with}
\alias{handle_request_with}
\title{Handle method for request trajectory}
\usage{
handle_request_with(.trj, resource, handle_function, handlers = 1)
}
\arguments{
\item{.trj}{request trajectory}

\item{resource}{resource name to seize and release}

\item{handle_function}{timeout function or handle time function}

\item{handlers}{number of discrete resources to be acquired}
}
\value{
request trajectory with handled resource
}
\description{
Handle method for request trajectory
}
