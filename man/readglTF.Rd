\name{readglTF}
\alias{readglTF}
\title{
Read a glTF file
}
\description{
The glTF file is the JSON part of a glTF representation of a 3D
scene.  This function reads one and returns a \code{"gltf"}
object containing the information in R format.

Typically most of the data for the scene is contained in other
files named in this one, usually found in the same file
folder.
}
\usage{
readglTF(path, defaultbin = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path}{
The path to the file being read.  R connections
cannot be used.
}
  \item{defaultbin}{
The name of the default associated binary file, if it is
not named in the JSON.  This is typically used when the JSON
has been extracted from a GLB file.
}
  \item{\dots}{
Not currently used.
}
}
\value{
An object of class \code{"gltf"}.
}
\references{
The specification of the glTF format:
\url{https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html}
}
\author{
Duncan Murdoch
}
\examples{
\donttest{

# This web page has lots of sample files

samples <- "https://raw.githubusercontent.com/KhronosGroup/glTF-Sample-Models/master/2.0"
filename <- tempfile(fileext = ".gltf")

# Get one of them:  a 2 cylinder engine.  We need both parts
# to be able to view it, though only the .gltf part is
# needed for readglTF()

download.file(paste0(samples, "/2CylinderEngine/glTF/2CylinderEngine.gltf"),
  destfile = filename)
download.file(paste0(samples, "/2CylinderEngine/glTF/2CylinderEngine0.bin?raw=true"),
  destfile = file.path(tempdir(), "2CylinderEngine0.bin"),
  mode = "wb")

gltf <- readglTF(filename)
gltf

# gltf files contain references to other files using
# relative paths, so we can only use them from their
# own directory
olddir <- setwd(dirname(filename))
rgl::plot3d(gltf)
setwd(olddir)

}
}
