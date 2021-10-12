plot3d.gltf <- function(x, ...) {
  rgl::shade3d(as.mesh3d(x, ...))
}
