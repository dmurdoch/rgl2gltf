playgltf <- function(gltf, animation = 0, start = times[1],
                     stop = times[2], times = gltf$timerange(animation), method = "wholeScene") {
  if (method != "wholeScene")
    stop("Only the 'wholeScene' method is implemented.")
  if (packageVersion("rgl") < "0.108.5")
    stop("glTF animation requires rgl 0.108.5 or higher")

  gltf$closeBuffers() # don't want to clone open connections
  clone <- gltf$clone()
  on.exit(clone$closeBuffers())

  clone$setParents()
  time <- start
  plot3d(clone, time = time, clone = FALSE)
  clockstart <- Sys.time()
  save <- par3d(skipRedraw = TRUE)
  on.exit(par3d(save), add = TRUE)
  repeat {
    time <- time + 1 # unclass(Sys.time() - clockstart + start)
    if (time > stop)
      break
    nodes <- clone$settime(time, animation)
    cat("Time = ", time, "\n")
    if (length(nodes)) {
      rgl::clear3d()
      plot3d(clone, time = time, add = TRUE, clone = FALSE)
      par3d(save)
      par3d(skipRedraw = TRUE)
    }
    Sys.sleep(0.1)
  }
}
