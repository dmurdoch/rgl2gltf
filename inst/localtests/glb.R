library(rgl2gltf)
# Okay:
plot3d(g1 <- readGLB("~/temp/Avocado.glb"))
plot3d(g2 <- readGLB("~/temp/BrainStem.glb"))
plot3d(g3 <- readGLB("~/temp/BarramundiFish.glb"))
plot3d(g4 <- readGLB("~/temp/NormalTangentTest.glb"))
plot3d(g5 <- readGLB("~/temp/2CylinderEngine.glb"))
plot3d(g6 <- readGLB("~/temp/AntiqueCamera.glb"))
plot3d(g7 <- readGLB("~/temp/RiggedSimple.glb"), zoom = 1.3)
