% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/iti_entity.R
\name{add_single_iti_entity}
\alias{add_single_iti_entity}
\alias{add_single_iti_entity.server_config}
\alias{add_single_iti_entity.scheduler_config}
\title{Add single IT infrastructure entity based on provided config}
\usage{
add_single_iti_entity(.env, .config)

\method{add_single_iti_entity}{server_config}(.env, .config)

\method{add_single_iti_entity}{scheduler_config}(.env, .config)
}
\arguments{
\item{.env}{IT infrastructure environment}

\item{.config}{config object}
}
\value{
IT infrastucture with added entity
}
\description{
Add single IT infrastructure entity based on provided config
}
\section{Methods (by class)}{
\itemize{
\item \code{server_config}: S3 Method for server_config

\item \code{scheduler_config}: S3 Method for scheduler_config
}}

