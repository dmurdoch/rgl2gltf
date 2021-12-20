playgltf <- function(gltf, animation = 0, start = times[1],
                     stop = times[2], times = gltf$timerange(animation), method = "wholeScene", ...) {
  if (method != "wholeScene")
    stop("Only the 'wholeScene' method is implemented.")
  if (packageVersion("rgl") < "0.108.5")
    stop("glTF animation requires rgl 0.108.5 or higher")

  time <- start
  plot3d(gltf, time = time, ...)
  clockstart <- Sys.time()
  save <- par3d(skipRedraw = TRUE)
  on.exit(par3d(save))
  repeat {
    time <- unclass(Sys.time() - clockstart + start)
    if (time > stop)
      break
    nodes <- gltf$settime(time, animation)
    if (length(nodes)) {
      rgl::clear3d()
      plot3d(gltf, time = time, add = TRUE)
      par3d(save)
      par3d(skipRedraw = TRUE)
    }
    Sys.sleep(0.1)
  }
}
