library(rgl)
library(rgl2gltf)

# obj <- gltfWidget(camera <- readGLB("~/svn/MyR/rgl2gltf/inst/localtests/AntiqueCamera.glb"))

obj <- gltfWidget(helmet <- readGLB("~/svn/MyR/rgl2gltf/inst/localtests/DamagedHelmet.glb"), usePBR = TRUE)

open3d()
clear3d("lights")

light3d(30, 30)

# obj <- gltfWidget(avocado <- readGLB("~/svn/MyR/rgl2gltf/inst/localtests/Avocado.glb"))

# obj <- gltfWidget(normaltest <- readGLB("~/svn/MyR/rgl2gltf/inst/localtests/NormalTangentTest.glb"), add = TRUE)


obj
