showTags <- function(tags = NULL, ids = NULL, subscenes = ids3d("subscene", subscene = 0)$id, depth_test = "always", ...) {

  currentsub <- currentSubscene3d()
  on.exit(useSubscene3d(currentsub))

  result <- NULL
  s <- scene3d()
  for (sub in subscenes) {
    useSubscene3d(sub)
    objs <- ids3d(subscene = sub, tags = TRUE)
    for (i in seq_len(nrow(objs))) {
      id <- objs$id[i]
      obj <- s$objects[[as.character(id)]]
      if (is.null(tag <- obj$material$tag))
        tag <- paste0("id", obj$id)
      if (!is.null(tags) && !(tag %in% tags))
        next
      if (!is.null(ids) && !(id %in% ids))
        next
      vertices <- obj$vertices
      ignoreExtent <- obj$ignoreExtent
      material <- list()
      material$margin <- obj$material$margin
      material$floating <- obj$material$floating

      useSubscene3d(sub)
      result <- c(result, structure(do.call(text3d, c(list(x = colMeans(vertices),
                                 texts=tag, depth_test = depth_test), material, list(...))), names = tag))
    }
  }
  lowlevel(result)
}
