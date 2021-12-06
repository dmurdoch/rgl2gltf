playgltf <- function(gltf, animation = 0, start = times[1],
                     stop = times[2], times = gltf$timerange(animation)) {
  if (packageVersion("rgl") < "0.108.5")
    stop("glTF animation requires rgl 0.108.5 or higher")
  clone <- gltf$clone()
  clone$setParents()
  time <- start
  plot3d(clone, time = time)
  clockstart <- Sys.time()
  save <- par3d(skipRedraw = TRUE)
  on.exit(par3d(save))
  repeat {
    time <- time + 1 # unclass(Sys.time() - clockstart + start)
    if (time > stop)
      break
    nodes <- clone$settime(time, animation)
    cat("Time = ", time, "\n")
    if (length(nodes)) {
      rgl::clear3d()
      plot3d(clone, time = time, add = TRUE)
      par3d(save)
      par3d(skipRedraw = TRUE)
    }
    Sys.sleep(0.1)
  }
}
