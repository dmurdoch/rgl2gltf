library(rgl)
library(rgl2gltf)

# gltfWidget(readGLB("~/svn/MyR/rgl2gltf/inst/localtests/AntiqueCamera.glb"))


# gltfWidget(readGLB("~/svn/MyR/rgl2gltf/inst/localtests/Avocado.glb"))

drawNormals <- function(vertices, normals) {
  n <- nrow(vertices)
  xyz <- matrix(NA, 2*n, 3)
  xyz[2*seq_len(n)-1,] <- vertices
  xyz[2*seq_len(n),] <- vertices + normals
  points3d(vertices)
  segments3d(xyz)
}

open3d()
clear3d("lights")

light3d(30, 30)
#points3d(saveobjmirror$vertices[1:20,])
# points3d(saveobj$vertices[3900:3920,])
obj <- gltfWidget(normaltest <- readGLB("~/svn/MyR/rgl2gltf/inst/localtests/NormalTangentTest.glb"), add = TRUE)
# drawNormals(obj$vertices, obj$tangents[,1:3]/10)

obj
