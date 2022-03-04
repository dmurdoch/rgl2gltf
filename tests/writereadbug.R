# This causes the following error on the very last line:

#> Error in transform %*% rbind(bbox[ix], bbox[iy], bbox[iz], 1): requires numeric/complex matrix/vector arguments

if (require(misc3d)) {

  library(rgl)
  library(rgl2gltf)

  mfrow3d(2,1)

  nmix3 <- function(x, y, z, m, s) {
    0.4 * dnorm(x, m, s) * dnorm(y, m, s) * dnorm(z, m, s) +
      0.3 * dnorm(x, -m, s) * dnorm(y, -m, s) * dnorm(z, -m, s) +
      0.3 * dnorm(x, m, s) * dnorm(y, -1.5 * m, s) * dnorm(z, m, s)
  }
  f <- function(x,y,z) nmix3(x,y,z,.5,.5)
  g <- function(n = 40, k = 5, alo = 0.1, ahi = 0.5, cmap = heat.colors) {
    th <- seq(0.05, 0.2, len = k)
    col <- rev(cmap(length(th)))
    al <- seq(alo, ahi, len = length(th))
    x <- seq(-2, 2, len=n)
    contour3d(f,th,x,x,x,color=col,alpha=al)
    bg3d(col="white")
  }
  g(40,5)
  next3d()
  g(40,4)
  s <- scene3d()
  gltf <- as.gltf(s)
  f <- tempfile(fileext = ".glb")
  writeGLB(gltf, f)
  gltf2 <- readGLB(f)
  plot3d(gltf2)
}
