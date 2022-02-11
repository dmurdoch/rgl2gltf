library(rgl2gltf)
library(rgl)
# gltf <- readGLB(system.file("glb/RiggedSimple.glb", package = "rgl2gltf"))

gltf <- readGLB("~/temp/BrainStem.glb")

playgltf(gltf, start = 0, stop = 0)
gltfWidget(gltf)


