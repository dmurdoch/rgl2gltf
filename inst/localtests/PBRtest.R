library(rgl)
library(rgl2gltf)

# obj <- gltfWidget(avocado <- readGLB("~/svn/MyR/rgl2gltf/inst/localtests/Avocado.glb"), usePBR = TRUE, add = TRUE)

obj <- gltfWidget(normaltest <- readGLB("~/svn/MyR/rgl2gltf/inst/localtests/NormalTangentTest.glb"))

obj

obj1 <- gltfWidget(helmet <- readGLB("~/svn/MyR/rgl2gltf/inst/localtests/DamagedHelmet.glb"))

obj1

obj2 <- gltfWidget(camera <- readGLB("~/svn/MyR/rgl2gltf/inst/localtests/AntiqueCamera.glb"))

obj2
