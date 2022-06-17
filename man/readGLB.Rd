\name{readGLB}
\alias{readGLB}
\title{
Read a GLB file.
}
\description{
GLB is the self-contained binary format of glTF
files.  This function reads one, extracts the embedded files
into the temporary directory, and returns a \code{"gltf"}
object containing the information in R format.
}
\usage{
readGLB(con, verbose = FALSE, ...)
}
\arguments{
  \item{con}{
The connection or filename to read from.
}
  \item{verbose}{
Whether to report on the process.
}
  \item{\dots}{
Currently unused.
}
}
\value{
An object of class \code{"gltf"}.  This is an R version of
the structure represented by a \dQuote{glTF} file's JSON
information.
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
\donttest{
# This web page has lots of sample files

samples <- "https://raw.githubusercontent.com/KhronosGroup/glTF-Sample-Models/master/2.0"

# Get one of them:  an avocado

gltf <- readGLB(paste0(samples, "/Avocado/glTF-Binary/Avocado.glb?raw=true"))

if (interactive())
  rgl::plot3d(gltf)

if (rgl::in_pkgdown_example())
  gltfWidget(gltf)

gltf$closeBuffers()
}
}
