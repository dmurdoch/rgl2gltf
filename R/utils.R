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
  if (!is.null(obj)) {
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

# Round a vector of doubles to single precision
toSingle <- function(x) {
  con <- rawConnection(raw(), "r+b")
  on.exit(close(con))
  writeBin(x, con, size = 4)
  seek(con, 0)
  readBin(con, "double", n = length(x), size=4)
}

# Compute min and max in single precision

minS <- function(x) min(toSingle(x))
maxS <- function(x) max(toSingle(x))

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


