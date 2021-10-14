\name{extractTexture}
\alias{extractTexture}
\title{
Extract a texture file from a gltf object
}
\description{
Extracts a texture from a gltf object, and writes it
to a file.
}
\usage{
extractTexture(gltf, index = 0,
               outfile = tempfile(),
               verbose = TRUE)
}
\arguments{
  \item{gltf}{
The gltf object.
}
  \item{index}{
The texture number (starting from 0).
}
  \item{outfile}{
The filename to write to.  If \code{outfile} has no
file extension, one will be added based on the MIME type
of the texture.
}
  \item{verbose}{
Whether to report on success.
}
}
\value{
Returns the filename that was written, or \code{NULL}
if the request failed.
}
