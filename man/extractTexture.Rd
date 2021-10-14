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
               verbose = TRUE,
               closeConnections = TRUE)
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
  \item{closeConnections}{
Close all connections in \code{gltf} on exit.
  }
}
\value{
Returns the filename that was written, or \code{NULL}
if the request failed.

If \code{closeConnections = FALSE}, the filename will have
attribute \code{"gltf"} containing the \code{gltf}
object which might now contain a new open connection.

If the texture has a recorded MIME type in \code{gltf},
that will be returned in attribute \code{"mimeType"}.
}
