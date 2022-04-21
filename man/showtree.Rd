\name{showtree}
\alias{showtree}
\alias{showtree.gltf}
\alias{showtree.rglscene}
\title{
Show the tree structure of nodes in a glTF object or rglscene
object.
}
\description{
glTF files contain nodes corresponding to objects and groups of objects
in 3D scenes.  They must be a tree or forest:  a node can only have
zero or one parent.  This function displays that structure,
or the similar structure for rglscene objects.
}
\usage{
showtree(x, ...)
\method{showtree}{gltf}(x, ...)
\method{showtree}{rglscene}(x, transform = FALSE, ...)
}
\arguments{
  \item{x}{
A \code{"gltf"} or \code{"rglscene"} object.
}
  \item{transform}{Whether to show the transform associated
  with a subscene.}
  \item{...}{Additional arguments, currently unused.}
}
