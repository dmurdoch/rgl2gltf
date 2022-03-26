writeglTF <- function(x, path, bin = TRUE) {
  if (bin) {
    basename <- tools::file_path_sans_ext(path)
    count <- 0
    for (i in seq_len(x$listCount("buffers"))) {
      buffer <- x$getBuffer(i-1)
      if (is.null(buffer$uri) && !is.null(buffer$bytes)) {
        buffername <- paste0(basename, count, ".bin")
        count <- count + 1
        writeBin(buffer$bytes, buffername)
        buffer$bytes <- NULL
        buffer$uri <- buffername
        x$setBuffer(i-1, buffer)
      }
    }
  }
  jsonlite::write_json(x$as.list(), path,
                       auto_unbox = TRUE,
                       digits = NA)
  invisible(path)
}

# The glTF format allows for multiple buffers, but this
# code only supports writing one, buffer 0.  It will be
# available in some combination of con, bytes, and uri.
# If neither con nor bytes is present, the buffer is in the
# external file named by uri.  If con is present, it holds the
# bytes, and if bytes is present, it does.  Normally only
# bytes will be there after con is closed.

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

  x$closeBuffers()

  # First get bytes if necessary

  bytes <- NULL
  if (x$listCount("buffers") > 0) {
    buffer <- x$getBuffer(0)
    uri <- buffer$uri
    bytes <- buffer$bytes
    if (!is.null(bytes))
      buffer$uri <- NULL
    buffer$bytes <- NULL
    x$setBuffer(0, buffer)
    # We'll restore x on exit
    on.exit({
      buffer <- x$getBuffer(0)
      buffer$uri <- uri
      buffer$bytes <- bytes
      x$setBuffer(0, buffer)
    })
  }

  # next create and write the JSON chunk

  jsonfile <- tempfile(fileext = ".gltf")
  writeglTF(x, jsonfile, bin = FALSE)
  on.exit(unlink(jsonfile), add = TRUE)

  if (is.character(con)) {
    con <- file(con, "wb")
    on.exit(close(con), add = TRUE)
  }
  writeBin(c(magic, 2L, 0L), con, size = 4, endian = "little")
  size <- 12 + writeChunk(jsonfile, typejson)

  # Next write the binary chunk.

  if (!is.null(bytes)) {
    binfile <- tempfile(fileext = ".bin")
    writeBin(bytes, binfile)
    on.exit(unlink(binfile), add = TRUE)
    size <- size + writeChunk(binfile, typebin)
  }
  seek(con, 8)
  writeBin(as.integer(size), con, size = 4, endian = "little")
  invisible(NULL)
}
