getTangents <- function(edges, vertices, normals, texcoords) {
  vertices <- xyz.coords(vertices)
  vertices <- rbind(vertices$x, vertices$y, vertices$z)
  nv <- ncol(vertices)

  normals <- xyz.coords(normals)
  normals <- rbind(normals$x, normals$y, normals$z)
  stopifnot(ncol(normals) == nv)

  texcoords <- xy.coords(texcoords)
  texcoords <- rbind(texcoords$x, texcoords$y)
  stopifnot(ncol(texcoords) == nv)

  tangents <- .C(C_get_tangents,
               as.integer(edges),
               as.integer(nv),
               as.double(vertices),
               as.double(normals),
               as.double(texcoords),
               tangents = double(4*nv))$tangents

  result <- matrix(tangents, ncol = 4, nrow = nv, byrow = TRUE,
         dimnames = list(NULL, c("x", "y", "z", "w")))
  result[,"w"] <- result[,"w"]

  result
}
