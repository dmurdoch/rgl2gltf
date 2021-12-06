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
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
gltf <- readGLB("~/temp/BrainStem.glb")
playgltf(gltf)
}
