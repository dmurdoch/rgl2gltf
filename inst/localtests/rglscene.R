library(rgl)
library(rgl2gltf)

g <- readGLB("~/temp/2CylinderEngine.glb")
s <- as.rglscene(g)
plot3d(s)

open3d()
m <- as.mesh3d(g)
shade3d(m)

