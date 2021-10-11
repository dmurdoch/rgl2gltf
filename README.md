# rgl2gltf
Read and write .gltf and .glb files

This package reads `.gltf` and `.glb` files containing 3D models, following the
[official spec at khronos.org](https://www.khronos.org/registry/glTF/specs/2.0/glTF-2.0.html) .

It is currently in very early development.  Please add issues or pull requests if something 
important to you is missing.

Sample code:

```r
library(rgl)
library(rgl2gltf)

gltf <- readGLB("https://github.com/KhronosGroup/glTF-Sample-Models/blob/master/2.0/2CylinderEngine/glTF-Binary/2CylinderEngine.glb?raw=true")
mesh <- as.mesh3d(gltf)
shade3d(mesh)
```
