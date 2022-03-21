getTangents <- function(obj) {
  edges <- switch(obj$type,
                  triangles = 3,
                  quads = 4,
                  NA)
  if (is.na(edges))
    stop("only triangles and quads are supported")

  indices <- obj$indices
  vertices <- obj$vertices
  nv <- nrow(vertices)
  if (is.null(indices))
    indices <- seq_len(nv)
  else
    indices <- as.integer(indices)
  vertices <- vertices[indices,]

  stopifnot(nrow(obj$normals) == nv)
  normals <- obj$normals[indices,]

  stopifnot(nrow(obj$texcoords) == nv)
  texcoords <- obj$texcoords[indices,]

  nv <- nrow(vertices)

  tangents <- .C(C_get_tangents,
               as.integer(edges),
               as.integer(nv),
               as.double(t(vertices)),
               as.double(t(normals)),
               as.double(t(texcoords)),
               tangents = double(4*nv))$tangents

  tangents <- matrix(tangents, ncol = 4, nrow = nv, byrow = TRUE,
         dimnames = list(NULL, c("x", "y", "z", "w")))

  if (nrow(obj$colors) > 1)
    newindices <- reindex(vertices = vertices,
                          normals = normals,
                          texcoords = texcoords,
                          tangents = tangents,
                          colors = obj$colors[indices,])
  else
    newindices <- reindex(vertices = vertices,
                          normals = normals,
                          texcoords = texcoords,
                          tangents = tangents)
  obj[names(newindices)] <- newindices
  obj
}
