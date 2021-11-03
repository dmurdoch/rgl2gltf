\name{showtree}
\alias{showtree}
\title{
Show the tree structure of nodes in a gltf object.
}
\description{
glTF files contain nodes corresponding to objects and groups of objects
in 3D scenes.  They must be a tree or forest:  a node can only have
zero or one parent.  This function displays the structure.
}
\usage{
showtree(gltf)
}
\arguments{
  \item{gltf}{
A \code{"gltf"} object.
}
}
