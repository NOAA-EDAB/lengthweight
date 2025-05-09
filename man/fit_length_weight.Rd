% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit_length_weight.R
\name{fit_length_weight}
\alias{fit_length_weight}
\title{Fit length weight relationship to species data from SVDBS}
\usage{
fit_length_weight(
  lengthWeightData,
  speciesName,
  outputDir = NULL,
  logfile,
  sex,
  season
)
}
\arguments{
\item{lengthWeightData}{Data frame. length-weight pairs. Each row represents an individual fish}

\item{speciesName}{Character string. Common name for species}
}
\value{
List of model fit objects
\item{commonSlope}{\code{\url{lm}} object. Fit for single slope (beta)}
\item{seasonalSlope}{\code{\url{lm}} object. Fit for seasonal slopes}
}
\description{
Fits a length weight relationship for use in length expansion
}
\section{Notes on model fitting }{


The simplest Null model (H0) is assumed to be
\deqn{W_i = \alpha L_i^\beta  exp(e_i)}

where W_i = Weight and L_i = Length of fish i, e_i ~ \eqn{N(0,\sigma^2)} and
\eqn{\alpha} &  \eqn{\beta} are intercept and slope parameters (on the log scale) to be estimated.

The alternative H1: \eqn{\beta_j != \beta} where \eqn{\beta_j} is the slope parameter (on the log scale) for season i.
i = 1, ..., 4 (spring, summer, fall, winter)

The above hypothesis is tested and the pvalue is output in the log file.
Plots of model fits are also produced and saved in the output directory
}

