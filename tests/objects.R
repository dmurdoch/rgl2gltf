library(rgl)
library(rgl2gltf)

xyz <- matrix(rnorm(36, sd=0.5), ncol = 3)
xyz <- scale3d(xyz, 1, 10, 100)
col <- rainbow(12)

plot3d(xyz, type="s")
points3d(xyz, col = col)
lines3d(xyz+1, col = col)
segments3d(xyz+2, col = col)
triangles3d(xyz+3, col = col)
quads3d(xyz+4, col = col)
particles3d(xyz+6, col = col, radius = 10)
tetra <- shade3d(scale3d(tetrahedron3d(), 0.5, 0.5, 0.5),
                 col = "red")
sprites3d(xyz+7, shapes=tetra)
aspect3d(1,1,1)
s <- scene3d()
s$rootSubscene$id
s$rootSubscene$par3d$listeners
plot3d(s)
g <- as.gltf(s)
s1 <- as.rglscene(g)
plot3d(s1)
