showNodes <- function(gltf, animation = 0, start = times[1], stop = times[2], times = gltf$timerange(animation)) {
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
  repeat {
    time <- unclass(Sys.time() - clockstart + start)
    if (time > stop)
      break
    gltf$settime(time)
    for (i in seq_along(subscenes)) {
      node <- as.numeric(sub("subscene", "", subnm[i]))
      par3d(userMatrix = gltf$getTransform(node),
            subscene = subscenes[i])
    }
    par3d(skipRedraw=FALSE)
    par3d(skipRedraw=TRUE)
  }
}
