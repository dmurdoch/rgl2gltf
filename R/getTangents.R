getTangents <- function(edges, indices, vertices, normals, texcoords) {
  ni <- length(indices)
  vertices <- xyz.coords(vertices)
  vertices <- rbind(vertices$x, vertices$y, vertices$z)
  nv <- ncol(vertices)
  stopifnot(all(indices > 0),
            all(indices <= nv))

  normals <- xyz.coords(normals)
  normals <- rbind(normals$x, normals$y, normals$z)
  stopifnot(ncol(normals) == nv)

  texcoords <- xy.coords(texcoords)
  texcoords <- rbind(texcoords$x, texcoords$y)
  stopifnot(ncol(texcoords) == nv)

  tangents <- .C(C_get_tangents,
               as.integer(edges),
               as.integer(ni),
               as.integer(nv),
               as.integer(indices - 1L),
               as.double(vertices),
               as.double(normals),
               as.double(texcoords),
               tangents = double(4*nv))$tangents

  matrix(tangents, ncol = 4, nrow = nv, byrow = TRUE,
         dimnames = list(NULL, c("x", "y", "z", "sign")))
}
