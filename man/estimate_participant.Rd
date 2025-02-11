% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimate_participant.R
\name{estimate_participant}
\alias{estimate_participant}
\title{Estimate the number of people needed and expected number of unique properties for a determined coverage based on the estimated norms}
\usage{
estimate_participant(est_norms, target_cover)
}
\arguments{
\item{est_norms}{a data frame with the estimated norms (generated by generateNorms)}

\item{target_cover}{float between 0 and 1, corresponding to coverage (the fraction of the total incidence probabilities of the reported properties that are in the reference sample)}
}
\value{
a vector with the extra number of participant to achieve the especific coverage, and the estimate of the number of unique properties listed by the new amount of suggested people
}
\description{
Estimate the number of people needed and expected number of unique properties for a determined coverage based on the estimated norms
}
\examples{
estimated_norms=generate_norms(data_test)
estimate_participant(estimated_norms,0.8)
}
