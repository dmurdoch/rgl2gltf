library(rgl)
library(rgl2gltf)

g <- readGLB(system.file("localtests/2CylinderEngine.glb", package = "rgl2gltf"))
s <- as.rglscene(g)
plot3d(s)

open3d()
m <- as.mesh3d(g)
shade3d(m)

