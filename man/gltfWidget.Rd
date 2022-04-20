\name{gltfWidget}
\alias{gltfWidget}
\title{
Create a widget for a glTF display.
}
\description{
This creates a widget holding a glTF scene, with controls
to animate it if it supports animation, and with
shaders that implement normal textures.
}
\usage{
gltfWidget(gltf,
           animation = 0,
           start = times[1], stop = times[2],
           times = gltf$timerange(animation),
           method = c("shader", "rigid"),
           add = FALSE, close = !add,
           verbose = FALSE,
           open3dParams = getr3dDefaults(),
           usePBR = hasPBRparams(gltf),
           PBRargs = list(), ...)
}
\arguments{
  \item{gltf}{
A \code{"gltf"} object, e.g. produced by \code{\link{readglTF}}.
}
  \item{animation}{
Which animation to use?  If no animation is present,
this is ignored.
}
  \item{start, stop}{
The starting and stopping times for the animation.  Ignored
if no animation.
}
  \item{times}{
An alternate way to specify the times.  Ignored if no
animation.
}
  \item{method}{
The
\code{"shader"} method installs a custom shader to implement
the animation in GLSL, as intended for glTF.  This is necessary
for the normal texture support.
The \code{"rigid"} method duplicates vertices as necessary so
that all triangles remain rigid as the vertices move, and
should match the corresponding method in \code{\link{playgltf}}.
}
  \item{add}{
Should the gltf object be added to an existing \pkg{rgl}
scene, or should it open a new scene?
}
  \item{close}{
Should \code{gltfWidget} close the \pkg{rgl} scene after
producing the widget?
}
  \item{verbose}{
Give some progress information.
}
  \item{open3dParams}{
A list to pass as the \code{params} argument to
\code{\link{open3d}}.
  }
  \item{usePBR}{
Whether to use physically based rendering methods.
The default
uses an internal function to determine if \code{gltf}
actually contains PBR parameters.
  }
  \item{PBRargs}{
A list containing optional arguments to the \code{\link{setPBRshaders}} function.
  }
  \item{\dots}{
Additional parameters which will be passed to \code{rgl::\link{playwidget}}.
}
}
\details{
See \code{\link{playgltf}} for a description of the method
used for animation.  The \code{"fixed"} method is fast, but
doesn't do a good job on some animations.

If the \code{gltf} object doesn't contain any animations,
or \code{animation = NA}, this
will simply display it as a widget with no controls.
}
\note{
Physically based rendering (PBR, controlled by \code{usePBR}) is
only used with \code{method = "shader"}.

If PBR is not used,
the \pkg{V8} package is required so that the shaders can be
modified.
}
\value{
A widget suitable for display or inclusion in an
R Markdown document.
}
\examples{
if ((interactive() || rgl::in_pkgdown_example()) && requireNamespace("manipulateWidget")) {
  gltf <- readGLB(system.file("glb/RiggedSimple.glb", package = "rgl2gltf"))
  gltfWidget(gltf)
}

}
