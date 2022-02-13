\name{gltfWidget}
\alias{gltfWidget}
\title{
Create a widget for an animated glTF display
}
\description{
This creates a widget holding a glTF scene, with controls
to animate it.
}
\usage{
gltfWidget(gltf,
           animation = 0,
           start = times[1], stop = times[2],
           times = gltf$timerange(animation),
           method = c("rigid"),
           add = FALSE, close = !add,
           verbose = FALSE,
           open3dParams = getr3dDefaults(),
           ...)
}
\arguments{
  \item{gltf}{
A \code{"gltf"} object, e.g. produced by \code{\link{readglTF}}.
}
  \item{animation}{
Which animation to use?
}
  \item{start, stop}{
The starting and stopping times for the animation.
}
  \item{times}{
An alternate way to specify the times.
}
  \item{method}{
Currently \code{"rigid"} is the only method supported.
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
\value{
A widget suitable for display or inclusion in an
R Markdown document.
}
