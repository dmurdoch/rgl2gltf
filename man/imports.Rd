\name{imports}
\alias{plot3d}
\alias{as.rglscene}
\alias{as.mesh3d}

\title{
Imports from rgl
}
\description{
These generic functions are imported from \pkg{rgl}.
}
\usage{
plot3d(x, ...)
as.rglscene(x, ...)
as.mesh3d(x, ...)
}
\arguments{
  \item{x}{
Objects to handle.
}
  \item{\dots}{
Additional arguments to pass to methods.
}
}
\details{
The main documentation for these generics is in \pkg{rgl}:
\code{\link[rgl]{plot3d}}.

This package defines \code{"gltf"} methods for the generics.
Documentation for \code{\link{as.rglscene.gltf}} is included.
}
