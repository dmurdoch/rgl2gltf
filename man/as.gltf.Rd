\name{as.gltf}
\alias{as.gltf}
\alias{as.gltf.mesh3d}
\alias{as.gltf.default}
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

\S3method{as.gltf}{default}(x, y = NULL, z = NULL, vertices,
                            material = NULL,
                            normals = NULL,
                            texcoords = NULL,
                            points = NULL, segments = NULL,
                            triangles = NULL,
                            quads = NULL,
                            ...,
                            previous = list(),
                            newScene = FALSE,
                            dir = tempdir())

\S3method{as.gltf}{mesh3d}(x, ...)
}
\arguments{
  \item{x}{
An object to convert to a \code{"gltf"} object.
}
  \item{y, z}{In the default method, combined with \code{x}
to make coordinates.  Any reasonable way of defining the coordinates is acceptable. See the function \code{\link{xyz.coords}} for details.}
    \item{vertices}{A 3 or 4 row matrix of Euclidean or homogeneous coordinates; takes
  precedence over \code{x, y, z}.}
  \item{material}{material properties for rendering}
  \item{normals}{normals at each vertex as a 3 or 4 \bold{column} matrix}
  \item{texcoords}{texture coordinates at each vertex as a 2 column matrix}
  \item{points}{vector of indices of vertices to draw as points}
  \item{segments}{2 x n matrix of indices of vertices to draw as
  segments}
  \item{triangles}{3 x n matrix of indices of vertices to draw
  as triangles}
  \item{quads}{4 x n matrix of indices of vertices to draw as quads},
  \item{\dots}{
Other parameters passed to the default method.
}
  \item{previous}{
Optionally a previously produced \code{"gltf"} object;
the new geometry will be added to it.
  }
\item{newScene}{logical; if \code{TRUE}, add a new
scene to \code{previous}, otherwise try to add to the existing
scene.}
  \item{dir}{
Where to write the binary buffer file.
}
}
\details{
\code{as.gltf} is a generic function.
}
\note{The arguments to the default method are similar to
the arguments to the \code{\link{mesh3d}} function, with a few
additional arguments after the dots.}
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
rgl::plot3d(gltf)
}
