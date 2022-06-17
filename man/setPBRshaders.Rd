\name{setPBRshaders}
\alias{setPBRshaders}
\title{
Set shaders for physically based rendering.
}
\description{
The glTF format is designed to hold objects which
are intended for "physically based rendering", where the
parameters of the object map to physical properties such
as metallicity, roughness, etc.  This function
replaces the default \pkg{rgl} shaders with PBR shaders
based on the reference implementation in
\url{https://github.com/KhronosGroup/glTF-Sample-Viewer/tree/88eda8c5358efe03128b72b6c5f5f6e5b6d023e1/shaders}.
}
\usage{
setPBRshaders(gltf, gltfMat, id,
              scene = scene3d(minimal = TRUE),
              useIBL = TRUE,
              brdfLUT = system.file("textures/brdfLUT.png", package = "rgl2gltf"),
              IBLspecular = system.file("textures/refmap.png", package = "rgl"),
              IBLdiffuse = system.file("textures/refmapblur.jpeg", package = "rgl2gltf"),
              debugBaseColor = 0,
              debugMetallic = 0,
              debugRoughness = 0,
              debugSpecularReflection = 0,
              debugGeometricOcclusion = 0,
              debugMicrofacetDistribution = 0,
              debugSpecContrib = 0,
              debugDiffuseContrib = 0,
              debugIBLDiffuse = 1,
              debugIBLSpecular = 1,
              defines = list(),
              uniforms = list(),
              attributes = list(),
              textures = list())
}
\arguments{
  \item{gltf, gltfMat}{
A \code{"gltf"} object, and a material record from it.
}
  \item{id, scene}{
The \pkg{rgl} id of the corresponding object and the scene holding it.
}
  \item{useIBL}{
Whether to use image based lighting.
}
  \item{brdfLUT}{
The texture to use for the "bidirectional reflectance distribution
function" lookup table.
}
  \item{IBLspecular}{
The texture to use for the "image based specular lighting".
}
  \item{IBLdiffuse}{
The texture to use for the "image based diffuse lighting".
}
  \item{debugBaseColor, debugMetallic, debugRoughness,
       debugSpecularReflection, debugGeometricOcclusion,
       debugMicrofacetDistribution, debugSpecContrib,
       debugDiffuseContrib}{
These are flags used for debugging.  Setting one of these to
1 will display just that contribution to the rendering.
       }
  \item{debugIBLDiffuse,
       debugIBLSpecular}{
Two more debugging settings.  These should be set to non-negative
values to control the contribution from each of those components
to the image based lighting.
}
  \item{defines, uniforms, attributes, textures}{
Values to use in \code{\link{setUserShaders}} in addition
to the ones determined by this function.
  }
}
\details{
\pkg{rgl} is designed to work with WebGL version 1, which doesn't
support all of the features used in the reference shaders.  In
particular, no extensions are assumed, and the IBL textures are
single 2D textures rather than cube maps.
}
\value{
This function modifies the \code{id} object in \code{scene},
and returns the modified scene.
}
\references{
\url{https://github.com/KhronosGroup/glTF-Sample-Viewer}
}
\author{
Duncan Murdoch for the adaptation to \pkg{rgl}, various
others for the original shaders.
}
\seealso{
\code{\link{gltfWidget}}
}
\examples{
\donttest{
# This web page has lots of sample files

samples <- "https://raw.githubusercontent.com/KhronosGroup/glTF-Sample-Models/master/2.0"

# Get one of them:  a 2 cylinder engine

gltf <- readGLB(paste0(samples, "/NormalTangentTest/glTF-Binary/NormalTangentTest.glb?raw=true"))
gltfMat <- gltf$getMaterial(0)
scene <- as.rglscene(gltf)
id <- scene$objects[[1]]$id
scene <- setPBRshaders(gltf, gltfMat, id, scene)
cat(scene$objects[[1]]$userFragmentShader)
}
}
