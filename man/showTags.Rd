\name{showTags}
\alias{showTags}
\title{
Debugging tool:  show tags for objects in rgl scene
}
\description{
This function uses \code{text3d} to display the tags
at the center of the objects they label.
}
\usage{
showTags(tags = NULL, ids = NULL,
         subscenes = ids3d("subscene", subscene = 0)$id,
         depth_test = "always", ...)
}
\arguments{
  \item{tags, ids}{
If non-NULL, display only these tags and ids.
}
  \item{subscenes}{
Which subscenes to examine.
}
  \item{depth_test}{
The \code{"depth_test"} material property to use on the
displayed tags.  The default will put the tag in front of
everything, so it won't be obscured by other objects.
}
  \item{\dots}{
Other arguments to pass to \code{\link[rgl]{text3d}}.
}
}
\details{
If selected objects don't have tags, they will be labelled
using their \code{id} value instead.
}
\value{
The \pkg{rgl} ids of the text objects added (one text object
per tagged object, with tags as names).
}
\examples{
example("plot3d", package = "rgl")
showTags()
}
