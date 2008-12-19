\name{hist.clinsig}
\alias{hist.clinsig}
\title{Plot a histogram of a clinsig list}
\description{
 Displays a histogram of the result of the \code{clinsig} function.
}
\usage{
 \method{hist}{clinsig}(x,breaks=NA,main="",xlab="Score",ylab="Frequency",
  xlim=NA,ylim=NA,col=2:3,border=par("fg"),...)
}
\arguments{
 \item{x}{a clinsig table produced by \code{\link{clinsig}}}
 \item{breaks}{The breaks to be used in categorizing scores.}
 \item{main}{The title for the plot.}
 \item{xlab,ylab}{The labels for the axes.}
 \item{xlim,ylim}{The limits for the plot.}
 \item{col}{The colors of the paired bars, pre- first, post- second.}
 \item{border}{The border color for the bars.}
 \item{...}{additional arguments passed to \code{plot}.}
}
\details{
 \code{hist.clinsig} displays a histogram of the counts of scores in the categories
 defined by \code{breaks} and other information in the object returned by \code{clinsig}.
}
\value{nil}
\author{Jim Lemon}
\seealso{\code{\link{clinsig}}}
\keyword{misc}
