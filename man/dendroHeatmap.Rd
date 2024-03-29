% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dendroHeatmap.R
\name{dendroHeatmap}
\alias{dendroHeatmap}
\title{Creates A Heatmap With Marginal Dendrogram(s)}
\usage{
dendroHeatmap(x, dendRow, dendCol = NULL, topDGadd = NULL,
  rightDGadd = NULL, topDGsize = 10, rightDGsize = 10, colLabels = NULL,
  ...)
}
\arguments{
\item{x}{the numerical matrix to be plotted}

\item{dendRow}{a \code{stats::\link[stats]{dendrogram}} object ordering the rows}

\item{dendCol}{a \code{stats::\link[stats]{dendrogram}} object ordering the columns}

\item{topDGadd}{top dendrogram, a list passed to \code{latticeExtra::\link[latticeExtra]{dendrogramGrob}()} as '\code{add}'}

\item{rightDGadd}{right dendrogram, a list passed to \code{latticeExtra::\link[latticeExtra]{dendrogramGrob}()} as '\code{add}'}

\item{topDGsize}{passed to \code{latticeExtra::\link[latticeExtra]{dendrogramGrob}()}}

\item{rightDGsize}{passed to \code{latticeExtra::\link[latticeExtra]{dendrogramGrob}()}}

\item{colLabels}{column labels}

\item{...}{parameters passed to \code{lattice::\link[lattice]{levelplot}()}, e.g. \code{main}, \code{par.settings}, ...}
}
\value{
an object of class "\code{trellis}"
}
\description{
Creates A Heatmap With Marginal Dendrogram(s)
}
\seealso{
\code{\link{Lattice}}, \code{lattice::\link[lattice]{levelplot}}
}
