plot3d.gltf <- function(x, scene = x$scene,
                        useRGLinfo = TRUE, time = NULL, clone = TRUE, ...) {
  plot3d(as.rglscene(x, scene = scene, useRGLinfo = useRGLinfo, time = time, clone = clone), ...)
}
