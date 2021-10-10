writeglTF <- function(x, path) {
  jsonlite::write_json(unclass(x), path, auto_unbox = TRUE)
}

writeGLB <- function(x, con) {
  magic <- 0x46546C67L
  typejson <- 0x4E4F534AL
  typebin  <- 0x004E4942L

  writeChunk <- function(filename, type) {
    filesize <- file.size(filename)
    chunksize <- bitwAnd(filesize + 3, bitwNot(3))
    bytes <- readBin(filename, "raw", n = filesize)
    writeBin(as.integer(c(chunksize, type)), con, endian = "little", size = 4)
    writeBin(bytes, con)
    if (chunksize > filesize) {
      if (type == typejson)
        writeBin(rep(as.raw(0x20), chunksize - filesize), con)
      else
        writeBin(rep(as.raw(0), chunksize - filesize), con)
    }
    chunksize + 8
  }

  if (length(x$buffers)) {
    buffer <- x$buffers[[1]]$uri
    x$buffers[[1]]$uri <- NULL
  } else
    buffer <- NULL

  jsonfile <- tempfile(fileext = ".gltf")
  writeglTF(x, jsonfile)
  if (is.character(con)) {
    con <- file(con, "wb")
    on.exit(close(con))
  }
  writeBin(c(magic, 2L, 0L), con, size = 4, endian = "little")
  size <- 12 + writeChunk(jsonfile, typejson)
  if (!is.null(buffer))
    size <- size + writeChunk(buffer, typebin)
  seek(con, 8)
  writeBin(as.integer(size), con, size = 4, endian = "little")
}
