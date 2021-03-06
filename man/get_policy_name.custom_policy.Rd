% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/custom_policy.r
\name{get_policy_name.custom_policy}
\alias{get_policy_name.custom_policy}
\title{Get policy name for custom policy}
\usage{
\method{get_policy_name}{custom_policy}(policy)
}
\arguments{
\item{policy}{policy object}
}
\value{
policy name with respect to "simmer"
}
\description{
In other words, there are 2 iterations of resources selection:
\enumerate{
\item Produce list of resource names. Production logic can encapsulate decision logic.
\item Simmer default algorithm selection.
}
}
