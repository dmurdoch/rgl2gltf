typeSignedByte    <- 5120
typeUnsignedByte  <- 5121
typeSignedShort   <- 5122
typeUnsignedShort <- 5123
typeSignedInt     <- 5124  # Not supported in glTF
typeUnsignedInt   <- 5125
typeFloat         <- 5126
typeDouble        <- 5130  # Not supported in glTF


Buffer <- R6Class("Buffer",
    public = list(

      initialize = function(json = NULL, binfile = NULL) {
        if (!is.null(json)) {
          private$buffers <- json$buffers
          private$bufferViews <- json$bufferViews
          private$accessors <- json$accessors
        }
        buffer <- self$getBuffer(0)
        if (is.null(buffer$uri))
          buffer$uri <- binfile
        self$setBuffer(0, buffer)
      },

      load = function(uri, buf = 0) {
        buffer <- self$getBuffer(buf)
        if (is.null(buffer))
          buffer <- list()
        if (!is.null(con <- buffer$con) &&
            inherits(con, "connection") &&
            isOpen(con)) {
          close(con)
        }
        if (is.character(uri)) {
          bytes <- readBin(uri, "raw", n = file.size(uri))
          buffer$uri <- uri
        } else if (is.raw(uri))
          bytes <- uri
        buffer$byteLength <- length(bytes)
        buffer$con <- rawConnection(bytes, open = "r+b")
        self$setBuffer(buf, buffer)
      },

      save = function(con, buf = 0) {
        buffer <- self$getBuffer(buf)
        if (is.null(buffer) ||
            is.null(con0 <- buffer$con) ||
            !inherits(con0, "connection") ||
            !isOpen(con0))
          stop("buffer ", buf, " is not open.")
        bytes <- rawConnectionValue(con0)
        writeBin(bytes, con)
      },

      getBuffer = function(buf, default = list()) {
        buffer <- if (buf + 1 <= length(private$buffers))
          private$buffers[[buf + 1]]
        if (is.null(buffer))
          default
        else
          structure(buffer, class = "gltfBuffer")
      },

      setBuffer = function(buf, buffer)
        private$buffers[[buf + 1]] <- buffer,

      openBuffer = function(buf) {
        buffer <- self$getBuffer(buf)
        if (is.null(buffer))
          stop("no such buffer")
        if (is.null(buffer$con)) {
          if (is.null(buffer$uri)) {
            buffer$con <- rawConnection(raw(0), open = "r+b")
            self$setBuffer(buf, buffer)
          } else
            self$load(buffer$uri, buf = buf)
        }
        self$getBuffer(buf)$con
      },

      closeBuffer = function(buf) {
        buffer <- self$getBuffer(buf)
        if (is.null(buffer))
          stop("no such buffer")
        if (!is.null(buffer$con)) {
          close(buffer$con)
          buffer$con <- NULL
          self$setBuffer(buf, buffer)
        }
      },

      getBufferview = function(bufv) {
        bufferview <- private$bufferViews[[bufv+1]]
        if (is.null(bufferview))
          stop("bufferView ", bufv, " not found.")
        structure(bufferview, class = "gltfBufferview")
      },

      openBufferview = function(bufv) {
        bufferview <- self$getBufferview(bufv)
        con <- self$openBuffer(bufferview$buffer)
        seek(con, bufferview$byteOffset)
        con
      },

      setBufferview = function(bufv, bufferView)
        private$bufferViews[[bufv + 1]] <- bufferView,

      getAccessor = function(acc)
        structure(private$accessors[[acc + 1]], class = "gltfAccessor"),

      readAccessor = function(acc) {
        typenames <- c("5120" = "byte", "5121" = "unsigned_byte",
                       "5122" = "short", "5123" = "unsigned_short",
                       "5125" = "unsigned_int", "5126" = "float")
        types <- c("5120" = "int", "5121" = "int",
                   "5122" = "int", "5123" = "int",
                   "5125" = "int", "5126" = "double")
        sizes <- c("5120" = 1, "5121" = 1,
                   "5122" = 2, "5123" = 2,
                   "5125" = 4, "5126" = 4)
        signeds <- c("5120" = TRUE, "5121" = FALSE,
                     "5122" = TRUE, "5123" = FALSE,
                     "5125" = TRUE, # not really, but make readBin happy
                     "5126" = TRUE)
        lens <- c(SCALAR = 1, VEC2 = 2, VEC3 = 3, VEC4 = 4,
                  MAT2 = 4, MAT3 = 9, MAT4 = 16)
        if (acc + 1 > length(private$accessors))
          stop("No such accessor")
        accessor <- self$getAccessor(acc)
        con <- self$openBufferview(accessor$bufferView)
        ctype <- as.character(accessor$componentType)
        atype <- accessor$type
        type <- types[ctype]
        len <- lens[atype]
        size <- sizes[ctype]
        signed <- signeds[ctype]
        count <- accessor$count
        if (is.null(view$byteStride)) {
          skip <- 0
        } else
          skip <- len*size - view$byteStride
        if (is.null(byteOffset <- accessor$byteOffset))
          byteOffset <- 0
        start <- seek(con) + byteOffset

        if (skip == 0) {
          seek(con, start)
          values <- readBin(con, type, n = len*count,  size = size,
                            signed = signed, endian = "little")
        } else {
          values <- numeric(count*len)
          for (i in seq_len(count)) {
            seek(con, start + (i-1)*view$byteStride)
            values[(i-1)*len + seq_len(len)] <-
              readBin(con, type, n = len,  size = size,
                      signed = signed, endian = "little")
          }
        }
        if (ctype == "5125") { # fix up unsigned integers
          values[is.na(values)] <- 2^31
          values[values < 0] <- values[values < 0] + 2^32
        }
        if (!is.null(accessor$normalized) && accessor$normalized)
          values <- switch(ctype,
                           "5120" = (values + 128)/255 - 1, # byte
                           "5121" = values/255,             # u byte
                           "5122" = (values + 2^15)/65535 - 1, # short
                           "5123" = values/65535,           # u short
                           values)                 # default
        if (len > 1)
          if (grepl("MAT", atype)) {
            values <- matrix(values, ncol = sqrt(len), byrow = TRUE)
          } else
            values <- matrix(values, ncol = len, byrow = TRUE)
        values
      },

      writeBuffer = function(values, type, size, buf = 0) {
        if (is.null(buffer <- self$getBuffer(buf)))
          self$setBuffer(buf, buffer <- list(byteLength = 0))
        byteLength <- buffer$byteLength
        byteOffset <- byteLength
        if (is.null(con <- buffer$con))
          con <- buffer$con <- rawConnection(raw(0), open = "r+b")
        seek(con, byteOffset)
        byteOffset <- bitwAnd(byteOffset + size - 1, bitwNot(size - 1))
        if (byteOffset > byteLength) {
          writeBin(raw(byteOffset - byteLength), con)
        }
        if (type %in% c(typeFloat, typeDouble))
          values <- as.numeric(values)
        else
          values <- as.integer(values)
        writeBin(values, con, size = size, endian = "little")
        buffer$byteLength <- byteOffset + length(values)*size
        self$setBuffer(buf, buffer)
        byteOffset
      },

      addBufferView = function(values, type, size, target = NULL, buf = 0) {
        bufferview <- list()
        bufferview$buffer <- buf
        bufferview$byteLength <- size*length(values)
        bufferview$byteOffset <- self$writeBuffer(values, type, size, buf)
        if (!is.null(target))
          bufferview$target <- target
        self$setBufferview(length(private$bufferViews), bufferview)
      },

      getType = function(x, useDouble = FALSE) {
        if (is.integer(x)) {
          r <- range(x)
          if (r[1] < 0) {
            if (-128 <= r[1] && r[2] <= 127)
              typeSignedByte
            else if (-32768 <= r[1] && r[2] <= 32767)
              typeSignedShort
            else
              typeSignedInt
          } else {
            if (r[2] <= 255)
              typeUnsignedByte
            else if (r[2] <= 65535)
              typeUnsignedShort
            else
              typeUnsignedInt
          }
        } else if (is.numeric(x)) {
          if (!useDouble)
            typeFloat
          else
            typeDouble
        } else
          stop('Unrecognized type')
      },

      addAccessor = function(values, target = NULL, glTF = TRUE, useDouble = FALSE) {
        componentType <- self$getType(values, useDouble)
        if (glTF) {
          if (componentType %in% c(typeSignedInt, typeDouble))
            stop("Type is not supported in glTF")
          if (any(!is.finite(values)))
            stop("Only finite values are supported in glTF")
        }
        if (componentType == typeFloat) {
          min <- minS  # These will only last for one call!
          max <- maxS
          size <- 4
        } else
          size <- switch(as.character(componentType),
            "5120" =,       # typeSignedByte
            "5121" = 1,     # typeUnsignedByte = 1,
            "5122" =,       # typeSignedShort =,
            "5123" = 2,     # typeUnsignedShort = 2,
            "5124" =,       # typeUnsignedInt =,
            "5125" =,       # typeSignedInt =,
            "5126" = 4,     # typeFloat = 4,
            "5130" = 8)     # typeDouble = 8)

        bufferView <- self$addBufferView(c(values), componentType,
                                    size = size, target = target)
        if (is.matrix(values)) {
          count <- ncol(values)
          type <- paste0("VEC", nrow(values))
          max <- I(apply(values, 1, max))
          min <- I(apply(values, 1, min))
          if (any(is.na(min)))
            browser()
        } else {
          count <- length(values)
          type <- "SCALAR"
          max <- I(max(values))
          min <- I(min(values))
        }
        accessor <- list(bufferView = bufferView,
                         componentType = componentType,
                         count = count,
                         type = type,
                         max = max,
                         min = min)
        private$accessors <- c(private$accessors, list(accessor))
        length(private$accessors) - 1
      },

      dataURI = function(buf = 0) {
        buffer <- self$getBuffer(buf)
        if (is.null(buffer))
          stop("Buffer ", buf, " does not exist.")
        con <- buffer$con
        if (is.null(con)) {
          if (is.null(buffer$uri))
            return(dataURI(raw(0), mime = "application/octet-stream"))
          else {
            self$load(buffer$uri, buf)
            buffer <- self$getBuffer(buf)
            con <- buffer$con
          }
        }
        bytes <- rawConnectionValue(con)
        base64enc::dataURI(bytes, mime = "application/octet-stream")
      }
  ),
  private = list(
    buffers = list(),
    bufferViews = list(),
    accessors = list()
  )
)
