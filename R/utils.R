# dot product of multiple pairs
dotprod <- function(x, y) {
  apply(x*y, 1, sum)
}

# dot product of one pair
dot <- function(v, w) sum(v*w)

# cross product of one pair
cross <- function(v, w) c( v[2]*w[3] - v[3]*w[2],
                           v[3]*w[1] - v[1]*w[3],
                           v[1]*w[2] - v[2]*w[1] )


normalize <- function(vertices) {
  lens <- sqrt(dotprod(vertices, vertices))
  vertices <- vertices/lens
  vertices
}

getDefaults <- function(class, value, default) {
  if (is.null(result <- getr3dDefaults(class, value)))
    result <- default
  result
}

# Is this an rgl object of a specified type?
# Works even if the class hasn't been restored yet.

isRGL <- function(obj, type) {
  if (!missing(obj) && !is.null(obj) && is.list(obj)) {
    rglclass <- paste0("rgl", type)
    identical(obj$class1, rglclass) ||
      identical(obj$type, type) ||
      inherits(obj, rglclass)
  } else
    FALSE
}

euclidean <- function(x, transpose = TRUE) {
  if (!is.null(x))
    if (any(is.na(x))) {
      warning("Cannot write NA values")
      NULL
    } else if (transpose)
      t(asEuclidean2(x))
  else asEuclidean(x)
}

# Compute normals for each face of a mesh and add them
addFaceNormals <- function(x) {
  x <- as.tmesh3d(x)

  vertices <- asEuclidean2(x$vb)
  normals <- NA*vertices
  texcoords <- x$texcoords
  it <- x$it
  for (i in seq_len(ncol(x$it))) {
    normal <- cross(vertices[,it[2,i]] - vertices[,it[1,i]],
                    vertices[,it[3,i]] - vertices[,it[2,i]])
    normal <- normal/sqrt(dot(normal, normal))
    for (j in 1:3) {
      if (is.na(normals[1, it[j, i]]))
        normals[, it[j, i]] <- normal
      else if (!isTRUE(all.equal(normal, normals[, it[j, i]]))) {
        # duplicate the vertex
        vertices <- cbind(vertices, vertices[,it[j, i]])
        normals <- cbind(normals, normal)
        if (!is.null(texcoords))
          texcoords <- cbind(texcoords, texcoords[,it[j, i]])
        it[j, i] <- ncol(vertices)
      }
    }
  }
  x$vb <- asHomogeneous2(vertices)
  x$normals <- asHomogeneous2(normals)
  if (!is.null(texcoords))
    x$texcoords <- texcoords
  x$it <- it
  x
}

# Prepare rgl* object for JSON
asRGLobj <- function(x) {
  x <- c(unclass(x), class=class(x))
  list(RGL_obj = x)
}

merge.rglobject <- function(x, y, ...) {
  for (n in setdiff(names(y), names(x)))
    x[[n]] <- y[[n]]
  if (is.null(x$material))
    x$material <- list()
  if (!is.null(y$material))
    for (n in setdiff(names(y$material), names(x$material)))
      x$material[[n]] <- y$material[[n]]
  x
}

is.multicolored <- function(mat)
  length(unique(mat$color)) > 1 || length(unique(mat$alpha)) > 1

matdiff <- function(mat1, mat2) {
  for (m in names(mat1)) {
    if (identical(mat1[[m]], mat2[[m]]))
      mat1[[m]] <- NULL
  }
  mat1
}

getFilter <- function(filter) {
  if (!is.null(filter))
    c("nearest" = 9728, "linear" = 9729,
      "nearest.mipmap.nearest" = 9984,
      "linear.mipmap.nearest" = 9985,
      "nearest.mipmap.linear" = 9986,
      "linear.mipmap.linear" = 9987)[filter]
}

tnonnull <- function(x)
  if(!is.null(x)) t(x)

# This function checks if a connection is still valid, since
# isOpen() doesn't work

isValidConnection <- function(con) {
  allconns <- showConnections()
  for (i in rownames(allconns)) {
    if (identical(con, getConnection(i)))
      return(TRUE)
  }
  FALSE
}

quaternionAngle <- function(q) {
  cosThetaby2 <- q[4]
  u <- quaternionAxis(q)
  i <- which.max(abs(u))
  sinThetaby2 <- if (u[i] == 0) 0 else
                 q[i]/u[i]
  2*atan2(sinThetaby2, cosThetaby2)
}

quaternionAxis <- function(q) {
  len <- sqrt(sum(q[1:3]^2))
  if (len == 0) c(1,0,0)
  else q[1:3]/len
}

transformBBox <- function(transform, bbox) {
  ix <- c(1, 1, 1, 1, 2, 2, 2, 2)
  iy <- c(3, 3, 4, 4, 3, 3, 4, 4)
  iz <- c(5, 6, 5, 6, 5, 6, 5, 6)
  xyz <- asEuclidean2(transform %*% rbind(bbox[ix], bbox[iy], bbox[iz], 1))
  c(min(xyz[1,]), max(xyz[1,]),
    min(xyz[2,]), max(xyz[2,]),
    min(xyz[3,]), max(xyz[3,]))
}

mergeBBox <- function(r1, r2) {
  if (!is.null(r2)) {
    i <- c(1, 3, 5)
    r1[i] <- pmin(r1[i], r2[i])
    i <- c(2, 4, 6)
    r1[i] <- pmax(r1[i], r2[i])
  }
  r1
}
