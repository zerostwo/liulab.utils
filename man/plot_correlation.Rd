% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/visualization.R
\name{plot_correlation}
\alias{plot_correlation}
\title{Visualize the correlation of two features}
\usage{
plot_correlation(
  adata,
  feature.x,
  feature.y,
  cutoff.x = -Inf,
  cutoff.y = -Inf,
  method = "pearson"
)
}
\arguments{
\item{adata}{Expression matrix, rows are cells and columns are features}

\item{feature.x}{features displayed on the x-axis}

\item{feature.y}{features displayed on the y-axis}

\item{cutoff.x}{Threshold for feature.x value}

\item{cutoff.y}{Threshold for feature.y value}

\item{method}{a character string indicating which correlation coefficient is
to be used for the test. One of "pearson", "kendall", or "spearman",
can be abbreviated.}
}
\value{
a list of ggplot objects
}
\description{
Visualize the correlation of two features
}
