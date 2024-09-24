\name{modifyShaders}
\alias{modifyShaders}
\title{
Edit shader code
}
\description{
This function applies specified edits to shader code
to support new functions.
}
\usage{
modifyShaders(shaders, mod, ...)
}
\arguments{
  \item{shaders}{
A list of shaders, e.g. as produced by \code{\link[rgl]{getShaders}}.
}
  \item{mod}{
Either a character value naming a built-in set of mods (currently just \code{"skins"}), or a list of mods as described
in the Details section.
}
  \item{\dots}{
Optional arguments; all will be passed to any functions
in \code{mods}.
}
}
\details{
The \code{mods} argument is organized in a nested list.
\enumerate{
\item{The top level can contain \code{vertexShader} and
\code{fragmentShader} components.}
\item{Each of those is a list of changes to apply to
that shader.}
\item{Each change is a list with \code{old} and \code{new}
entries.}
\item{The \code{old} entry is used as a "fixed" pattern to
select one or more lines from the shader to be replaced.
Multiple matches are allowed, but they shouldn't overlap.
Changes will be applied in the order specified, so take care
that a pattern doesn't match new text from an earlier change.}
\item{The \code{new} entry contains the replacement.  It
can either be a character vector or a function that returns
a character vector.  If it is a function, it will be passed
the \code{...} argument.

See \code{rgl2gltf:::shaderChanges} for the built-in modifications.
}
}
}
\value{
A new shader object incorporating the edits.
}
