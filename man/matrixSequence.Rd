\name{matrixSequence}
\alias{matrixSequence}
\alias{print.matrixSequence}
\title{
Print the sequence of matrices affecting an object.
}
\description{
This is used for debugging, to see how an object within
nested \pkg{rgl} subscenes is affected by their user matrices.
}
\usage{
matrixSequence(tag, scene = scene3d())
\method{print}{matrixSequence}(x, n = 5, ...)
}
\arguments{
  \item{tag}{
Which objects to report on?
}
  \item{scene}{
The \pkg{rgl} scene to work from.
}
  \item{x}{
Object to print.
  }
  \item{n}{
In the print method, how many vertices and indices should be printed?
  }
  \item{...}{ Ignored. }
}
\value{
A list containing records with entries as follows:
\item{id}{The object id}
\item{vertices}{The object's vertices}
\item{indices}{The object's indices}
\item{userMatrix}{A list of user matrices affecting this object}
}
