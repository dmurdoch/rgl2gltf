
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rgl2gltf: Read and write .gltf and .glb files

This package contains functions to read and write `.gltf` and `.glb`
files containing 3D models, following the [official spec at
khronos.org](https://www.khronos.org/registry/glTF/specs/2.0/glTF-2.0.html)
.

It is currently in very early development. Please add issues or pull
requests if something important to you is missing.

Sample code:

``` r
library(rgl)
library(rgl2gltf)

gltf <- readGLB("https://github.com/KhronosGroup/glTF-Sample-Models/blob/master/2.0/2CylinderEngine/glTF-Binary/2CylinderEngine.glb?raw=true")
mesh <- as.mesh3d(gltf)
open3d()
#> glX 
#>   1
shade3d(mesh)
rglwidget()
#> Warning in snapshot3d(scene = x, width = width, height = height): webshot = TRUE
#> requires the webshot2 package; using rgl.snapshot() instead
```

![](/var/folders/d6/s97fjjxd3_9353x_lwb692100000gn/T//RtmpJrnSek/filed4e367723f9.png)<!-- -->
