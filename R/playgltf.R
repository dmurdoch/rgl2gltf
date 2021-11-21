playgltf <- function(gltf, animation = 0, start = times[1],
                     stop = times[2], times = gltf$timerange(animation)) {
  gltf <- gltf$clone()
  plot3d(gltf)
  clockstart <- Sys.time()
  save <- par3d(skipRedraw = TRUE)
  on.exit(par3d(save))
  repeat {
    time <- unclass(Sys.time() - clockstart + start)
    if (time > stop)
      break
    nodes <- gltf$settime(animation, time)
    cat("Time = ", time, ": ")
    print(nodes)
    if (length(nodes)) {
      plot3d(gltf)
      par3d(save)
      par3d(skipRedraw = TRUE)
    }
    Sys.sleep(0.1)
  }
}
