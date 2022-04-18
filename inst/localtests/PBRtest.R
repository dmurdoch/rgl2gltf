library(rgl)
library(rgl2gltf)

open3d()
clear3d("lights")

light3d(30, 30)

# obj <- gltfWidget(avocado <- readGLB("~/svn/MyR/rgl2gltf/inst/localtests/Avocado.glb"), usePBR = TRUE, add = TRUE)

obj <- gltfWidget(normaltest <- readGLB("~/svn/MyR/rgl2gltf/inst/localtests/NormalTangentTest.glb"), add = TRUE)

obj

open3d()
clear3d("lights")

light3d(30, 30)

obj1 <- gltfWidget(helmet <- readGLB("~/svn/MyR/rgl2gltf/inst/localtests/DamagedHelmet.glb"), add = TRUE)

obj1

open3d()
clear3d("lights")

light3d(30, 30)

obj2 <- gltfWidget(camera <- readGLB("~/svn/MyR/rgl2gltf/inst/localtests/AntiqueCamera.glb"), add = TRUE)

obj2
