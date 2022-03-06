library(rgl2gltf)
library(rgl)

gltf <- readGLB(system.file("glb/RiggedSimple.glb", package = "rgl2gltf"))

asRow(
gltfWidget(gltf, method = "rigid"),
if (requireNamespace("V8"))
  gltfWidget(gltf, method = "shader"))
