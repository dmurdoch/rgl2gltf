library(rgl2gltf)
library(rgl)

gltf <- readGLB("~/temp/BrainStem.glb")
#gltf <- readGLB(system.file("glb/RiggedSimple.glb", package = "rgl2gltf"))

asRow(
gltfWidget(gltf, method = "rigid"),
gltfWidget(gltf, method = "shader"))
