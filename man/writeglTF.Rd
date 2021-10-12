\name{writeglTF}
\alias{writeglTF}
\alias{writeGLB}
\title{
Write a glTF or GLB file.
}
\description{
GLB is the self-contained binary format of glTF
files.  These functions write a \code{"gltf"}
object to one of these formats.
}
\usage{
writeglTF(x, path)
writeGLB(x, con)
}
\arguments{
  \item{x}{
A \code{"gltf"} object, e.g. from \code{\link{as.gltf}}.
}
  \item{path}{
A filename in which to write the JSON part of the file.
}
  \item{con}{
A filename or connection to which to write the GLB file.
  }
}
\value{
Both functions return \code{NULL} invisibly.
}
\references{
The specification of the glTF format:
\url{https://www.khronos.org/registry/glTF/specs/2.0/glTF-2.0.html}
}
\author{
Duncan Murdoch
}
\seealso{
\code{\link{readglTF}}
}
\examples{
filename <- tempfile(fileext = ".glb")
writeGLB(as.gltf(rgl::cube3d(col = "red")), filename)
}
