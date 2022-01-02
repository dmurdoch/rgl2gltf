\name{playgltf}
\alias{playgltf}
\alias{showNodes}
\title{
Play an animated glTF object.
}
\description{
\file{.gltf} and \file{.glb} files can contain animation instructions.
This function interprets them and plays them.
}
\usage{
playgltf(gltf, animation = 0,
         start = times[1], stop = times[2],
         times = gltf$timerange(animation),
         method = c("wholeScene", "partialScene"), ...)
showNodes(gltf, animation = 0,
          start = times[1], stop = times[2],
          times = gltf$timerange(animation))
}
\arguments{
  \item{gltf}{
A \code{"gltf"} object.
}
  \item{animation}{
Which animation from the object?  An integer from 0 to the
number of animations defined in \code{gltf}.
}
  \item{start, stop}{
Starting and stopping times.
}
  \item{times}{
An alternate way to specify \code{start} and \code{stop}.
}
  \item{method}{
Which drawing method to use?  See details below.
  }
  \item{...}{
Parameter settings to pass to \code{\link{plot3d.rglscene}} (and hence
to \code{\link{open3d}}).
  }
}
\details{
GLTF files are animated by time dependent changes to the
transformations in their nodes.  Those transformations
correspond to RGL \code{par3d("userMatrix")} settings in
subscenes and can sometimes be directly imported as such.

However, GLTF files also support "skins", a computer graphics
concept not supported in RGL.  A skin is a way to say that
different vertices of the same object (typically a triangle mesh)
respond to different nodes.  This allows shapes to be stretched,
similar to skin on a moving body.  RGL assumes that all
graphics objects are rigid.

The \code{playgltf} function provides partial support for
skins.  Using the \code{"wholeScene"} method, it can modify the vertices of an entire scene
and redraw the scene.  Typically this is quite slow, and
not very satisfactory.  The \code{"partialScene"} method
allows only the
changed objects to be redrawn, which might help speed things up.
Future plans are to allow the skins to be made rigid, but allow a
single transformation to be changed in real time, or to
implement the skins entirely in shaders, which is how they
are intended to be implemented, but which is not currently well supported
by \pkg{rgl}.

The \code{showNodes} function displays each node number as
text at the origin for that node.  By default it plays the
animation showing how the nodes move.

}
\value{
Called for the side effect of drawing the animation.
}

\examples{
gltf <- readGLB("~/temp/BrainStem.glb")
showNodes(gltf, start = 0, stop = 3)
playgltf(gltf, start = 0, stop = 3)
}
