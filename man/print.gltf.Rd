\name{print.gltf}
\alias{print.gltf}
\title{
Print gltf objects.
}
\description{
Prints \code{"gltf"} objects with various levels of detail.
}
\usage{
\S3method{print}{gltf}(x, verbose = FALSE, names = FALSE, ...)
}

\arguments{
  \item{x}{
The object to print.
}
  \item{verbose}{
A character vector naming the sections that should be printed
in full detail, or a logical value, with \code{TRUE} indicating
all sections should be printed verbosely.
}
  \item{names}{
A character vector naming the sections for which object names
should be printed, if present.
}
  \item{\dots}{
Currently ignored.
}
}
\details{
The recognized section names for \code{verbose} are:
\code{"accessors", "asset", "scene", "scenes", "nodes",
"buffers", "bufferViews", "meshes", "cameras", "materials",
"textures"} and \code{"images"}.  Unique prefixes will be
recognized.
}
\value{
The \code{x} object is printed and then returned invisibly.
}
\examples{
samples <- "https://raw.githubusercontent.com/KhronosGroup/glTF-Sample-Models/master/2.0"

gltf <- readGLB(paste0(samples, "/2CylinderEngine/glTF-Binary/2CylinderEngine.glb?raw=true"))

print(gltf, names = "meshes")
}
