library(rgl)
library(rgl2gltf)

avocado <- readGLB("~/svn/MyR/rgl2gltf/inst/localtests/Avocado.glb")

gltfWidget(avocado)
