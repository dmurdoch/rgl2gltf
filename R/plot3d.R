plot3d.gltf <- function(x, scene = x$scene, decorate = FALSE,
                        showRGLinfo = TRUE, ...)
  plot3d(as.rglscene(x, scene = scene, showRGLinfo = showRGLinfo), decorate = decorate, ...)
