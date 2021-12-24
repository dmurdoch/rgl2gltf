library(rgl2gltf)
# Okay:
plot3d(g1 <- readGLB(system.file("localtests/Avocado.glb", package = "rgl2gltf")))
plot3d(g2 <- readGLB(system.file("localtests/BrainStem.glb", package = "rgl2gltf")))
plot3d(g3 <- readGLB(system.file("localtests/BarramundiFish.glb", package = "rgl2gltf")))
plot3d(g4 <- readGLB(system.file("localtests/NormalTangentTest.glb", package = "rgl2gltf")))
plot3d(g5 <- readGLB(system.file("localtests/2CylinderEngine.glb", package = "rgl2gltf")))
plot3d(g6 <- readGLB(system.file("localtests/AntiqueCamera.glb", package = "rgl2gltf")))
plot3d(g7 <- readGLB(system.file("glb/RiggedSimple.glb", package = "rgl2gltf")), zoom = 1.3)
