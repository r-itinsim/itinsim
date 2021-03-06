% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/policy.r
\name{new_policy}
\alias{new_policy}
\title{Simmer policy wrapper}
\usage{
new_policy(
  name = character(),
  resources = character(),
  ...,
  class = character(),
  initial_container = list(),
  is_simmer_policy = TRUE
)
}
\arguments{
\item{name}{policy name (either default or custom)}

\item{resources}{vector of resource names to enumerate over}

\item{...}{additional arguments}

\item{class}{custom class name, used for extensions}

\item{initial_container}{container of items from which policy object is going to be created}

\item{is_simmer_policy}{defines whether wrapper should hold simmer policy in order to match policy name or no}
}
\value{
new policy object wrapper
}
\description{
Simmer policy wrapper
}
