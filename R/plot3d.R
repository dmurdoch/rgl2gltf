plot3d.gltf <- function(x, scene = x$scene, decorate = FALSE, ...)
  plot3d(as.rglscene(x, scene = scene), decorate = decorate, ...)
