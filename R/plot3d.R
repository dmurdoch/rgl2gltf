plot3d.gltf <- function(x, scene = x$scene, decorate = FALSE,
                        useRGLinfo = TRUE, ...)
  plot3d(as.rglscene(x, scene = scene, useRGLinfo = useRGLinfo), decorate = decorate, ...)
