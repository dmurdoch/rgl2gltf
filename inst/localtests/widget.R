library(rgl)
library(rgl2gltf)

gltf <- readGLB("~/temp/AntiqueCamera.glb")

gltfWidget(gltf)
