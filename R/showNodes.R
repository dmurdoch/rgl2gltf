showNodes <- function(gltf, animation = 0, start = times[1], stop = times[2], times = gltf$timerange(animation), speed = 1, by = NULL) {
  gltf <- gltf$clone()
  gltf$settime(start)
  s <- as.rglscene(gltf)
  objs <- plot3d(s, skipRedraw = TRUE)
  on.exit(par3d(skipRedraw = FALSE))
  nm <- names(objs)
  subscenes <- objs[grepl("subscene", nm)]
  subnm <- names(subscenes)
  res <- c()
  for (i in seq_along(subscenes)) {
    node <- sub("subscene", "", subnm[i])
    useSubscene3d(subscenes[i])
    res <- c(res, text3d(0,0,0, node))
  }
  for (i in objs) {
    if (!(i %in% subscenes))
      pop3d(id = i)
  }

  clockstart <- Sys.time()
  duration <- as.difftime(stop - start, units = "secs")
  time <- start
  repeat {
    if (is.null(by))
      time <- (Sys.time() - clockstart)*speed
    else
      time <- time + by
    if (time > duration)
      break

    time <- as.numeric(time, units = "secs") + start

    gltf$settime(time, animation)

    for (i in seq_along(subscenes)) {
      node <- as.numeric(sub("subscene", "", subnm[i]))
      par3d(userMatrix = gltf$getTransform(node),
            subscene = subscenes[i])
    }
    par3d(skipRedraw=FALSE)
    par3d(skipRedraw=TRUE)
  }
}
