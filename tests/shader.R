library(rgl2gltf)
library(rgl)

gltf <- readGLB(system.file("glb/RiggedSimple.glb", package = "rgl2gltf"))

if (requireNamespace("manipulateWidget"))
  asRow(gltfWidget(gltf, method = "rigid"),
        gltfWidget(gltf, method = "shader"))
