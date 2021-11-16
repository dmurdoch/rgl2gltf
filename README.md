
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rgl2gltf: Read and write .gltf and .glb files

This R package contains functions to read, write and display `.gltf` and
`.glb` files containing 3D models, following the [official spec at
khronos.org](https://www.khronos.org/registry/glTF/specs/2.0/glTF-2.0.html)
. It also contains functions to convert `rgl` `mesh3d` objects and
scenes to and from glTF objects.

It is currently in active development. Please add issues or pull
requests if something important to you is missing.

A `pkgdown` website is here: <https://dmurdoch.github.io/rgl2gltf/dev>

To install, run

``` r
remotes::install_github("dmurdoch/rgl2gltf")
```

Sample code:

``` r
library(rgl)
library(rgl2gltf)

gltf <- readGLB("https://github.com/KhronosGroup/glTF-Sample-Models/blob/master/2.0/2CylinderEngine/glTF-Binary/2CylinderEngine.glb?raw=true")
mesh <- as.mesh3d(gltf)
open3d(windowRect = c(0, 0, 500, 400))
#> glX 
#>   1
shade3d(mesh)
snapshot3d(webshot = FALSE, filename = "man/figures/engine.png")
```

![](man/figures/engine.png)
