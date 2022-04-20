\name{getTangents}
\alias{getTangents}
\title{
Use the MikkTSpace code to generate tangent vectors.
}
\description{
This function generates tangent vectors using the MikkTSpace
code by Morten S. Mikkelsen.
}
\details{
glTF files include normal textures, which require the tangent
space to be specified:  the normals at each vertex are
supplemented with a tangent vector and a bitangent vector
that is their cross product.  The standard recommends
that if the glTF file doesn't specify tangents, they should
be generated using the MikkTSpace code.

Note that a comment in \file{mikktspace.h} indicates that
indexing needs to be recalculated after computing the tangents,
so this function works on unindexed inputs, and reapplies
indexing at the end.
}
\usage{
getTangents(obj)
}
\arguments{
  \item{obj}{
A triangles or quads object as returned by \code{rgl::scene3d()}.
}
}
\value{
A modified copy of the original object, adding a 4 column
\code{tangents} entry.  The order and number of indices
may have changed.
}
\references{
The MikkTSpace code was obtained from \url{https://github.com/mmikk/MikkTSpace}.  The interface code is based on
code from \url{https://www.turais.de/using-mikktspace-in-your-project/} by Johannes Kuehnel.
}
\author{
Morten S. Mikkelsen, Johannes Kuehnel and Duncan Murdoch.
}
