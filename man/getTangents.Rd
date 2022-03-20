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
}
\usage{
getTangents(edges, indices, vertices, normals, texcoords)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{edges}{
How many edges to each face?  Must be 3 or 4 for triangles
or quads.
}
  \item{indices}{
Indices used for the vertices.
}
  \item{vertices, normals}{
The vertices and normals as 3 column matrices, dataframes or other structures
handled by \code{\link{xyz.coords}}.
}
  \item{texcoords}{
The texture coordinates as 2 columns handled by \code{\link{xy.coords}}.
}
}
\value{
A three column matrix holding the tangent vectors at each
vertex.
}
\references{
The MikkTSpace code was obtained from \url{https://github.com/mmikk/MikkTSpace}.  The interface code is based on
code from \url{https://www.turais.de/using-mikktspace-in-your-project/} by Johannes Kuehnel.
}
\author{
The MikkTSpace code by Morten S. Mikkelsen was obtained from
\url{https://github.com/mmikk/MikkTSpace}.  The interface code
is based on
code from \url{https://www.turais.de/using-mikktspace-in-your-project/} by Johannes Kuehnel.
}
