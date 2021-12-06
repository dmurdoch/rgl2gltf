plot3d.gltf <- function(x, scene = x$scene, decorate = FALSE,
                        useRGLinfo = TRUE, time = NULL, ...)
  plot3d(as.rglscene(x, scene = scene, useRGLinfo = useRGLinfo, time = time), decorate = decorate, ...)
