\name{as.gltf}
\alias{as.gltf}
\alias{as.gltf.mesh3d}
\title{
Produce glTF objects
}
\description{
The glTF file is the JSON part of a glTF representation of a 3D
scene.  This function creates the R structure corresponding to
one, and writes the binary buffer file for it.
}
\usage{
as.gltf(x, ...)
\S3method{as.gltf}{mesh3d}(x, result = list(), newScene = FALSE, dir = tempdir(), ...)
}
\arguments{
  \item{x}{
An object to convert to a \code{"gltf"} object.
}
  \item{result}{
Optionally a previously produced \code{"gltf"} object;
the new geometry will be added to it.
  }
  \item{newScene}{logical; if \code{TRUE}, add a new
scene to \code{result}, otherwise try to add to the existing
scene.}
  \item{dir}{
Where to write the binary buffer file.
}
  \item{\dots}{
Other parameters.
}
}
\details{
\code{as.gltf} is a generic function.
}
\value{
A \code{"gltf"} object.
}
\references{
The specification of the glTF format:
\url{https://www.khronos.org/registry/glTF/specs/2.0/glTF-2.0.html}
}
\author{
Duncan Murdoch
}
\examples{
cube <- rgl::cube3d(col = "red")
gltf <- as.gltf(cube)
rgl::as.mesh3d(gltf)
}
