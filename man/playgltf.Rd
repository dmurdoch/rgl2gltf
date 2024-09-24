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
         method = c("rigid", "wholeScene", "partialScene"),
         speed = 1, by = NULL, verbose = FALSE, ...)
showNodes(gltf, animation = 0,
          start = times[1], stop = times[2],
          times = gltf$timerange(animation),
          speed = 1, by = NULL)
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
  \item{speed, by}{
Control the updates; see details below.
  }
  \item{verbose}{
Whether to print status updates.
  }
  \item{...}{
Parameter settings to pass to \code{\link[rgl]{plot3d.rglscene}} (and hence
to \code{\link[rgl]{open3d}}).
  }
}
\details{
glTF files are animated by time dependent changes to the
transformations in their nodes.  Those transformations
correspond to RGL \code{par3d("userMatrix")} settings in
subscenes and can sometimes be directly imported as such.

However, glTF files also support "skins", a computer graphics
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
Finally, the \code{"rigid"} method converts all polygons
to rigid ones that are supported by \pkg{rgl}, so that motion
is done by changes to the transformations.  This is likely the fastest
method, but for some animations the errors introduced by the
conversion are unacceptably large.

The \code{showNodes} function displays each node number as
text at the origin for that node.  By default it plays the
animation showing how the nodes move.

For both functions, the \code{speed} and \code{by} arguments
specify the \dQuote{times} at which the animation is drawn.  If
\code{by} is specified, then a frame is drawn at time \code{start} and
subsequent frames increment the time by \code{by}.  If it is
\code{NULL} (the default), then the \code{speed} argument is used
as a multiplier on the internal time (taken to be in seconds).
For example, with the default \code{speed = 1}, the first frame will
be drawn at time \code{start}, and when it is complete, the next one
will be drawn according to how many seconds have passed in real time,
etc.

}
\value{
Called for the side effect of drawing the animation.
}

\examples{
\donttest{
if (interactive() && !rgl::in_pkgdown_example()) {
# This example is fast enough using the "whole" method:

gltf1 <- readGLB(system.file("glb/RiggedSimple.glb", package = "rgl2gltf"))
playgltf(gltf1, start = 0, stop = 3, method = "whole")

# It looks terrible using the "rigid" method, because some triangles
# need to be deformed:

playgltf(gltf1, start = 0, stop = 3, method = "rigid")

# This example is too slow using anything but "rigid", but it's fine there:

samples <- "https://raw.githubusercontent.com/KhronosGroup/glTF-Sample-Models/master/2.0"
gltf2 <- readGLB(paste0(samples, "/BrainStem/glTF-Binary/BrainStem.glb?raw=true"))
playgltf(gltf2, start = 0, stop = 2, speed = 0.25, method = "rigid")
}
}
}
