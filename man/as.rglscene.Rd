\name{as.rglscene.gltf}
\alias{as.rglscene.gltf}
\alias{as.mesh3d.gltf}
\title{
Convert a glTF object to an rglscene or mesh3d object.
}
\description{
These methods convert a \code{"gltf"} object to
a \code{"rglscene"} object, similar to what \code{\link{scene3d}}
would produce, or a \code{"mesh3d"} object.
}
\usage{
\S3method{as.rglscene}{gltf}(x, scene = x$scene, nodes = NULL,
         useRGLinfo = TRUE, time = NULL, ani = 0, clone = TRUE,
         quick = FALSE, add = FALSE, ...)
\S3method{as.mesh3d}{gltf}(x, ...)
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
  \item{useRGLinfo}{\code{"gltf"} objects contain RGL-specific information
in \dQuote{extra} fields.  If \code{useRGLinfo} is \code{TRUE}, we use
that information, otherwise include only what standard glTF viewers would display.}
  \item{time}{Set the "time" within an animation.}
  \item{ani}{Animation number to use.}
  \item{clone}{Whether to clone the \code{gltf} object.  See the Details below.}
  \item{quick}{If \code{TRUE}, \code{\link{plot3d}} will work
  on the result, but it is not sufficiently complete to use
  as the scene in \code{\link{rglwidget}}.}
  \item{add}{If \code{quick = FALSE}, the scene will be plotted
  in an existing rgl scene, and both old and new parts will be returned.}
  \item{...}{\code{as.mesh3d} passes these arguments to \code{as.rglscene}.}
}
\details{
These functions need to modify the glTF object, caching
some information to help with the conversion.  By default
they do this on a cloned copy of it, and the original is
left unchanged.  If the object has already been cloned
this can be skipped by setting \code{clone = FALSE}.
}
\value{
An \code{"rglscene"} object.
}
