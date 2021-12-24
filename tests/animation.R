library(rgl2gltf)
library(rgl)
gltf <- readGLB(system.file("glb/RiggedSimple.glb", package = "rgl2gltf"))

plot3d(gltf, time=0, zoom = 1.3)
save <- par3d(skipRedraw = TRUE)
for (time in seq(0, 2, by=0.04)) {
  clear3d()
  plot3d(gltf, time = time, add = TRUE)
  par3d(save)
  par3d(skipRedraw=TRUE)
}
par3d(save)
