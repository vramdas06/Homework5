## HW5 Class/Methods

setClass(
    Class = "sparse_numeric",
    slots = c(
        value = "numeric",
        pos = "integer",
        length = "integer"
    )
)

### Validity Method ###

setValidity("sparse_numeric", function(object) {
  if (length(object@value) != length(object@pos)) {
    return("Lengths of 'value' and 'pos' must be equal.")
  }
  if (any(object@pos < 1L) || any(object@pos > object@length)) {
    return("'pos' values must be between 1 and 'length'.")
  }
  if (anyDuplicated(object@pos)) {
    return("'pos' must not contain duplicates.")
  }
  if (!is.numeric(object@value)) {
    return("'value' must be numeric.")
  }
  if (!is.integer(object@length)) {
    return("'length' must be an integer")
  }
  TRUE
})

### Create Arithmetic Generics ###
setGeneric("sparse_add", function(x, y, ...) standardGeneric("sparse_add"))
setGeneric("sparse_sub", function(x, y, ...) standardGeneric("sparse_sub"))
setGeneric("sparse_mult", function(x, y, ...) standardGeneric("sparse_mult"))
setGeneric("sparse_crossprod", function(x, y, ...) standardGeneric("sparse_crossprod"))


### Addition Method ###

setMethod("sparse_add", c("sparse_numeric", "sparse_numeric"),
  function(x, y) {
    if (x@length != y@length) stop("Vectors must have the same length.")

    # union of non-zero positions
    all_pos <- sort(unique(c(x@pos, y@pos)))
    if (length(all_pos) == 0) {
      return(new("sparse_numeric", value = numeric(0), pos = integer(0), length = x@length))
    }

    # match all_pos into x and y to get indices (NA if missing)
    ix <- match(all_pos, x@pos) # index into x@value or NA
    iy <- match(all_pos, y@pos) # index into y@value or NA

    # fetch values only for matched indices; replace NA with 0
    xvals <- x@value[ix]
    xvals[is.na(ix)] <- 0
    yvals <- y@value[iy]
    yvals[is.na(iy)] <- 0

    res_vals <- xvals + yvals
    keep <- res_vals != 0
    if (!any(keep)) {
      return(new("sparse_numeric", value = numeric(0), pos = integer(0), length = x@length))
    }

    new("sparse_numeric",
        value = res_vals[keep],
        pos = as.integer(all_pos[keep]),
        length = x@length)
  })


### sparse_sub ###
setMethod("sparse_sub", c("sparse_numeric", "sparse_numeric"),
          function(x, y) {
            if (x@length != y@length)
              stop("Vectors must have the same length.")

            all_pos <- sort(unique(c(x@pos, y@pos)))
            x_values <- rep(0, length(all_pos))
            y_values <- rep(0, length(all_pos))

            x_values[match(x@pos, all_pos)] <- x@value
            y_values[match(y@pos, all_pos)] <- y@value

            result_vals <- x_values - y_values
            keep <- result_vals != 0

            new("sparse_numeric",
                value = result_vals[keep],
                pos = as.integer(all_pos[keep]),
                length = x@length)
          })

### sparse_mult ###
setMethod("sparse_mult", c("sparse_numeric", "sparse_numeric"),
          function(x, y) {
            if (x@length != y@length)
              stop("Vectors must have the same length.")

            common_pos <- intersect(x@pos, y@pos)
            if (length(common_pos) == 0) {
              return(new("sparse_numeric", value = numeric(0), pos = integer(0), length = x@length))
            }

            x_vals <- x@value[match(common_pos, x@pos)]
            y_vals <- y@value[match(common_pos, y@pos)]

            result_vals <- x_vals * y_vals
            keep <- result_vals != 0

            new("sparse_numeric",
                value = result_vals[keep],
                pos = as.integer(common_pos[keep]),
                length = x@length)
          })

### sparse_crossprod ##
setMethod("sparse_crossprod", c("sparse_numeric", "sparse_numeric"),
          function(x, y) {
            if (x@length != y@length)
              stop("Vectors must have the same length.")
            common <- intersect(x@pos, y@pos)
            sum(x@value[match(common, x@pos)] * y@value[match(common, y@pos)])
          })

### Operator overloading ###
setMethod("+", c("sparse_numeric", "sparse_numeric"), function(e1, e2) sparse_add(e1, e2))
setMethod("-", c("sparse_numeric", "sparse_numeric"), function(e1, e2) sparse_sub(e1, e2))
setMethod("*", c("sparse_numeric", "sparse_numeric"), function(e1, e2) sparse_mult(e1, e2))


### Coercion Methods ###

# numeric -> sparse_numeric
setAs("numeric", "sparse_numeric", function(from) {
  nonzero_idx <- which(from != 0)
  new("sparse_numeric",
      value = from[nonzero_idx],
      pos = as.integer(nonzero_idx),
      length = as.integer(length(from)))
})

# sparse_numeric -> numeric
setAs("sparse_numeric", "numeric", function(from) {
  out <- numeric(from@length)
  out[from@pos] <- from@value
  out
})

### Show method ###
setMethod("show", "sparse_numeric", function(object) {
  cat("Sparse numeric vector of length", object@length, "\n")
  cat("Non-zero elements:\n")
  print(data.frame(pos = object@pos, value = object@value))
})

### Plot Method ###
setMethod("plot", c("sparse_numeric", "sparse_numeric"), function(x, y, ...) {
  if (x@length != y@length)
    stop("Vectors must have the same length.")

  plot(x@pos, x@value, col = "blue", pch = 16, xlab = "Position", ylab = "Value",
       main = "Non-zero Elements of Sparse Vectors", ...)
  points(y@pos, y@value, col = "red", pch = 17)
  legend("topright", legend = c("x", "y"), col = c("blue", "red"), pch = c(16, 17))
})

### Additional Method ###
setGeneric("sparse_mean", function(x) standardGeneric("sparse_mean"))

setMethod("sparse_mean", "sparse_numeric", function(x) {
  sum(x@value) / x@length
})

