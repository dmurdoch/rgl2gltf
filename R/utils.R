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

mergeMaterial <- function(x, y) {
  n <- setdiff(names(y), names(x))
  x[n] <- y[n]
  x
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

quaternionToMatrix <- function(q) {
  xx <- q[1]^2
  xy <- q[1]*q[2]
  xz <- q[1]*q[3]
  xw <- q[1]*q[4]
  yy <- q[2]^2
  yz <- q[2]*q[3]
  yw <- q[2]*q[4]
  zz <- q[3]^2
  zw <- q[3]*q[4]
  matrix(c(1 - 2*(yy + zz),
           2*(xy + zw),
           2*(xz - yw),
           0,
             2*(xy - zw),
             1 - 2*(xx + zz),
             2*(yz + xw),
             0,
               2*(xz + yw),
               2*(yz - xw),
               1 - 2*(xx + yy),
               0,
                 0,
                 0,
                 0,
                 1), 4,4)
}

quaternionAxis <- function(q) {
  len <- sqrt(sum(q[1:3]^2))
  if (len == 0) c(1,0,0)
  else q[1:3]/len
}

getObjBBox <- function(obj) {
  bbox <- NULL
  if (!is.null(obj$vertices)) {
    vertices <- obj$vertices
    vertices <- vertices[complete.cases(obj$vertices),,drop = FALSE]
    if (nrow(vertices))
      bbox <- as.numeric(apply(vertices, 2, range))
  }
}

bboxRadius <- function(bbox, scale) {
  diag <- (bbox[c(2,4,6)] - bbox[c(1,3,5)])*scale/2
  sqrt(sum(diag^2))
}

transformBBox <- function(transform, bbox) {
  if (is.null(bbox) || !all(is.finite(bbox)))
    return(bbox)
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

# Find subscene chains containing a given ID
findSubscenes <- function(s, id) {

  addSubscenes <- function(sub, chain, transform) {
    chain <- c(chain, sub$id)
    transform <- transform %*% sub$par3d$userMatrix
    if (id %in% sub$objects)
      result <<- c(result, list(list(chain, transform)))
    for (child in sub$subscenes)
      addSubscenes(child, chain, transform)
  }

  result <- list()
  addSubscenes(s$rootSubscene, numeric(0), diag(4))
  result
}

weightedTransform <- function(joint, weight, forward, backward){
  wt <- weight/sum(weight)
  transform <- matrix(0, 4,4)
  for (j in seq_along(joint)) {
    if (wt[j] == 0) next
    transform <- transform + wt[j] * forward[,,joint[j] + 1] %*% backward[,,joint[j]+1]
  }
  transform
}

# Get the sequence of userMatrix transformations applying
# to a particular tag

# The same tag may appear in more than one place, so this
# needs to be a list.

matrixSequence <- function(tag, scene = scene3d()) {
  # First, find the id(s) for that tag
  ids <- integer()
  objs <- scene$objects
  for (i in seq_along(objs)) {
    if (!is.null(thistag <- objs[[i]]$material$tag) &&
        tag == thistag)
      ids <- c(ids, objs[[i]]$id)
  }
  # Now walk through the subscenes
  recurse <- function(sub) {
    result <- list()
    children <- sub$subscenes
    for (i in seq_along(children)) {
      downstream <- recurse(children[[i]])
      result <- c(result, downstream)
    }
    # Add records if any ids are here
    hits <- intersect(sub$objects, ids)
    for (i in seq_along(hits)) {
      obj <- scene$objects[[as.character(hits[i])]]
      here <- list(id = hits[i],
                   vertices = obj$vertices,
                   indices = obj$indices)
      result <- c(result, list(here))
    }
    # Add this subscene's info to results
    for (i in seq_along(result)) {
      result[[i]]$userMatrix <- c(list(sub$par3d$userMatrix), result[[i]]$userMatrix)
      names(result[[i]]$userMatrix)[1] <- sub$id
    }
    result
  }
  structure(recurse(scene$rootSubscene),
            class = "matrixSequence")
}

print.matrixSequence <- function(x, n = 5, ...) {
  for (i in seq_along(x)) {
    cat("Id: ", x[[i]]$id, ":", paste(head(x[[i]]$indices, n), collapse = ", "), ", ...\n")
    print(round(head(x[[i]]$vertices, n), 3))
    userMatrix <- x[[i]]$userMatrix
    for (j in seq_along(userMatrix)) {
      cat("Matrix ", names(userMatrix)[j], ":\n")
      print(round(userMatrix[[j]], 3))
    }
    cat("Transformed:\n")
    vertices <- rbind(t(x[[i]]$vertices), 1)
    for (j in rev(seq_along(userMatrix)))
      vertices <- userMatrix[[j]] %*% vertices
    vertices <- t(asEuclidean2(vertices))
    print(round(head(vertices, n), 3))
  }
}

# Restoring from JSON turns some vectors and matrices
# into lists; these functions restore them.

fixVector <- function(v, vNames) {
  if (!is.null(v)) {
    v <- unlist(v)
    if (is.null(names(v)))
      names(v) <- vNames
  }
  v
}

fixMatrix <- function(m, rowNames, colNames) {
  if (is.list(m) && is.list(m[[1]])) {
    m <- matrix(unlist(m), length(m), length(m[[1]]), byrow = TRUE)
    if (is.null(dimnames(m)))
      dimnames(m) <- list(rowNames, colNames)
  }
  m
}

par3dNames <- list(
  viewport = c("x", "y", "width", "height"),
  mouseMode = c("none", "left", "right", "middle", "wheel")
)
xyzNames <- c("x", "y", "z")
attribVNames <- list(
  embeddings = c("viewport", "projection", "model", "mouse")
)
attribColNames <- list(
  vertices = xyzNames,
  normals = xyzNames,
  colors = c("r", "g", "b", "a"),
  texcoords = c("s", "t"),
  dim = c("r", "c"),
  adj = xyzNames,
  centers = xyzNames,
  usermatrix = c("x", "y", "z", "w"),
  axes = xyzNames)
attribRowNames <- list(
  axes = c("mode", "step", "nticks", "marklen", "expand")
)

fixList <- function(l, vectors = NULL, matrices = NULL, nulls = NULL, vNames = list(), rowNames = list(), colNames = list()) {
  if (!is.null(l)) {
    for (n in vectors)
      if (!is.null(l[[n]]))
        l[[n]] <- fixVector(l[[n]], vNames[[n]])
    for (n in matrices)
      if (!is.null(l[[n]]))
        l[[n]] <- fixMatrix(l[[n]], rowNames[[n]], colNames[[n]])
    for (n in nulls)
      if (!is.null(l[[n]]) && !length(l[[n]]))
          l[[n]] <- NULL
  }
  l
}
