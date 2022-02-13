library(rgl)
library(rgl2gltf)

set.seed(123)

xyz <- matrix(rnorm(36, sd=0.5), ncol = 3)
xyz <- scale3d(xyz, 1, 10, 100)
col <- rainbow(12)

open3d()
plot3d(xyz, type="s")
points3d(xyz+5, col = col)
lines3d(xyz+1, col = col)
segments3d(xyz+2, col = col)
triangles3d(xyz+3, col = col)
quads3d(xyz+4, col = col)
particles3d(xyz+6, col = col, radius = 10)
tetra <- shade3d(tetrahedron3d(),
                 col = "red")
sprites3d(xyz+7, shapes=tetra, radius = 5)
aspect3d(1,1,1)
s <- scene3d()
# plot3d(s)
g <- as.gltf(s)
s1 <- as.rglscene(g)
plot3d(s1)
s2 <- as.rglscene(g, useRGLinfo = FALSE)
plot3d(s2)
s1 <- scene3d()
w1 <- rglwidget(s1)
s3 <- s2
w3 <- rglwidget(s3)
w3

plot3d(s3)
s4 <- scene3d()
w4 <- rglwidget(s4)
