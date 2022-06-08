library(rgl2gltf)
library(rgl)
gltf <- readGLB(system.file("glb/RiggedSimple.glb", package = "rgl2gltf"))

playgltf(gltf, start = 0, stop = 0)
if (requireNamespace("manipulateWidget"))
  gltfWidget(gltf)
