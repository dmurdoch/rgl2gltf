library(rgl2gltf)
library(rgl)

# Tests from R to glTF

x <- cube3d(col="red")
open3d(); shade3d(x)
bg3d("yellow")

s <- scene3d()
plot3d(s)
# This opens a window; perhaps it should do next3d() instead

g1 <- as.gltf(s)
s1 <- as.rglscene(g1)
plot3d(s1)
plot3d(g1)

# Has bogus new window, due to calling plot3d.rglscene
# add = TRUE puts it in the wrong place

g2 <- as.gltf(x)
s2 <- as.rglscene(g2)
plot3d(g2)

example(plot3d, package="rgl")
s3 <- scene3d()
g3 <- as.gltf(s3)
plot3d(g3)
