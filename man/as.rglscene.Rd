\name{as.rglscene}
\alias{as.rglscene.gltf}
\alias{as.mesh3d.gltf}
\title{
Convert a gltf object to an rglscene or mesh3d object.
}
\description{
These methods convert a \code{"gltf"} object to
a \code{"rglscene"} object, similar to what \code{\link{scene3d}}
would produce, or a \code{"mesh3d"} object.
}
\usage{
\S3method{as.rglscene}{gltf}(x, scene = x$scene, nodes = NULL, ...)
\S3method{as.mesh3d}{gltf}(x, scene = x$scene, nodes = NULL, ...)
}
\arguments{
  \item{x}{
The \code{"gltf"} object to convert.
}
  \item{scene}{Which scene to convert?  If \code{NULL} (e.g.
\code{x} doesn't define a default scene), scene 0 will be used.
}
  \item{nodes}{Which nodes to convert? If \code{NULL}, all
  nodes used in the scene will be converted, otherwise only
  the listed ones and their children.
  }
  \item{...}{Currently unused.}
}
\value{
An \code{"rglscene"} object.
}
