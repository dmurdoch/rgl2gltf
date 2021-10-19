library(rgl)
library(rgl2gltf)

g <- readGLB("~/temp/2CylinderEngine.glb")
s <- as.rglscene(g)
plot3d(s)

open3d()
m <- as.mesh3d(g)
shade3d(m)
# oldscene <- scene3d()
# oldlight <- rgl.ids("lights")$id
# oldpar3d <- list(par3d(), oldscene$par3d)
#
# x <- oldscene$objects[[1]]$normals
# y <- newscene$objects[[1]]$normals[newscene$objects[[1]]$indices,]
#waldo::compare(x,y)
