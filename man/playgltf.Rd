\name{playgltf}
\alias{playgltf}
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
         method = "wholeScene", ...)
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
Which drawing method to use?  Currently only \code{"wholeScene"} is implemented.
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
not very satisfactory.  Future plans will allow only the
changed objects to be redrawn, which might help speed things up,
or to make the skins rigid, but allow a single transformation to be changed in real time.  It remains to be seen which
approach will be preferred.

}
\value{
Called for the side effect of drawing the animation.
}

\examples{
gltf <- readGLB("~/temp/BrainStem.glb")
playgltf(gltf, start = 0, stop = 3)
}
