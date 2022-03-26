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
writeglTF(x, path, bin = TRUE)
writeGLB(x, con)
}
\arguments{
  \item{x}{
A \code{"gltf"} object, e.g. from \code{\link{as.gltf}}.
}
  \item{path}{
A filename in which to write the JSON part of the file.
}
  \item{bin}{logical; whether or not to write the binary
part of the object.}
  \item{con}{
A filename or connection to which to write the GLB file.
  }
}
\details{
If \code{bin = TRUE} (the default),
\code{writeglTF} will write the binary part of the object
to one or more separate files with filename constructed by
concatenating the main part of \code{path}, followed by a number
and the extension \file{.bin}.
}
\value{
\code{writeglTF} returns \code{path} invisibly.
\code{writeGLB} returns \code{NULL} invisibly.
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
