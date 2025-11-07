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

### Addition Method ###
setGeneric("sparse_add", function(x, y, ...) standardGeneric("sparse_add"))

setMethod("sparse_add", c("sparse_numeric", "sparse_numeric"),
  function(x, y) {
    # Error when x and y are different lengths
    if (x@length != y@length)
      stop("Vectors must have the same length.")

    # combine and sort all positions
    all_pos <- sort(unique(c(x@pos, y@pos)))
    if (length(all_pos) == 0) { # No non-zero elements in either x or y
      return(new("sparse_numeric", value = numeric(0), pos = integer(0), length = x@length))
    }

    # get indices of all_pos where x is non 0
    ix <- match(all_pos, x@pos)
    # get indices of all_pos where y is non 0
    iy <- match(all_pos, y@pos)

    # get values where X and y are non-zero, otherwise fill with 0
    xvals <- x@value[ix]
    xvals[is.na(ix)] <- 0
    yvals <- y@value[iy]
    yvals[is.na(iy)] <- 0

    # Sum the shortened sparse vectors
    res_vals <- xvals + yvals
    keep <- res_vals != 0 # drop any 0s that are created
    if (!any(keep)) { # if all previously non-zero elements become 0, return an all 0 sparse vector
      return(new("sparse_numeric", value = numeric(0), pos = integer(0), length = x@length))
    }

    # Return the resulting sparse vector
    new("sparse_numeric",
        value = res_vals[keep],
        pos = as.integer(all_pos[keep]),
        length = x@length)
  })


### sparse_sub ###
setGeneric("sparse_sub", function(x, y, ...) standardGeneric("sparse_sub"))

setMethod("sparse_sub", c("sparse_numeric", "sparse_numeric"),
          function(x, y) {
            # Error when x and y are different lengths
            if (x@length != y@length)
              stop("Vectors must have the same length.")

            # combine and sort all positions
            all_pos <- sort(unique(c(x@pos, y@pos)))

            # create all 0 value vectors for x and y separately
            x_values <- rep(0, length(all_pos))
            y_values <- rep(0, length(all_pos))

            #fill in proper values where x and y are non-zero
            x_values[match(x@pos, all_pos)] <- x@value
            y_values[match(y@pos, all_pos)] <- y@value

            # subtract the vectors
            result_vals <- x_values - y_values
            keep <- result_vals != 0 # index of non-zeroes in resulting value vector

            # create the resulting vector
            new("sparse_numeric",
                value = result_vals[keep],
                pos = as.integer(all_pos[keep]),
                length = x@length)
          })

### sparse_mult ###
setGeneric("sparse_mult", function(x, y, ...) standardGeneric("sparse_mult"))

setMethod("sparse_mult", c("sparse_numeric", "sparse_numeric"),
          function(x, y) {
            # Error when x and y are different lengths
            if (x@length != y@length)
              stop("Vectors must have the same length.")

            # only find overlapping positions
            common_pos <- intersect(x@pos, y@pos)

            # all elements will be 0 if x and y have no common positions
            if (length(common_pos) == 0) {
              return(new("sparse_numeric", value = numeric(0), pos = integer(0), length = x@length))
            }

            # make small vector with values at the common positions for x and y
            x_vals <- x@value[match(common_pos, x@pos)]
            y_vals <- y@value[match(common_pos, y@pos)]

            # multiply and keep non-zero results
            result_vals <- x_vals * y_vals
            keep <- result_vals != 0

            # create the resulting sparse vector
            new("sparse_numeric",
                value = result_vals[keep],
                pos = as.integer(common_pos[keep]),
                length = x@length)
          })

### sparse_crossprod ##
setGeneric("sparse_crossprod", function(x, y, ...) standardGeneric("sparse_crossprod"))

setMethod("sparse_crossprod", c("sparse_numeric", "sparse_numeric"),
          function(x, y) {
            # Error when x and y are different lengths
            if (x@length != y@length)
              stop("Vectors must have the same length.")
            # only find overlapping positions
            common <- intersect(x@pos, y@pos)

            # sum the product of overlapping position values
            sum(x@value[match(common, x@pos)] * y@value[match(common, y@pos)])
          })

### Operator overloading ###
setMethod("+", c("sparse_numeric", "sparse_numeric"), function(e1, e2) sparse_add(e1, e2))
setMethod("-", c("sparse_numeric", "sparse_numeric"), function(e1, e2) sparse_sub(e1, e2))
setMethod("*", c("sparse_numeric", "sparse_numeric"), function(e1, e2) sparse_mult(e1, e2))


### Coercion Methods ###

# numeric -> sparse_numeric
setAs("numeric", "sparse_numeric", function(from) {
  nonzero_idx <- which(from != 0) # get indices of non-zeor elements
  new("sparse_numeric",
      value = from[nonzero_idx],
      pos = as.integer(nonzero_idx),
      length = as.integer(length(from)))
})

# sparse_numeric -> numeric
setAs("sparse_numeric", "numeric", function(from) {
  out <- numeric(from@length) # create numeric vector of length l
  out[from@pos] <- from@value # replace non-zero elements with values
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

  # Find overlapping nonzero positions
  common_pos <- intersect(x@pos, y@pos)

  # when there are no overlapping positions
  if (length(common_pos) == 0) {
    plot(0, 0, type = "n", xlab = "Position", ylab = "Value",
         main = "No overlapping non-zero elements")
    return(invisible(NULL))
  }

  # get values for overlapping positions
  ix <- match(common_pos, x@pos)
  iy <- match(common_pos, y@pos)
  xvals <- x@value[ix]
  yvals <- y@value[iy]

  # Plot setup
  plot(range(common_pos), range(c(xvals, yvals)),
       type = "n",
       xlab = "Position",
       ylab = "Value",
       main = "Overlapping Non-Zero Elements by Position")

  # Add x and y points (different color, size, and shape + transparency to see overlap better)
  points(common_pos, xvals, pch = 17, col = rgb(0, 0, 1, 0.5), cex = 1.5)
  points(common_pos, yvals, pch = 16, col = rgb(1, 0, 0, 0.5))

  # Add legend
  legend("topright",
         legend = c("x values", "y values"),
         col = c(rgb(0, 0, 1, 0.6), rgb(1, 0, 0, 0.6)),
         pch = c(17, 16),
         bty = "n")
})

### Additional Method - mean of the sparse vector ###
setGeneric("sparse_mean", function(x) standardGeneric("sparse_mean"))

setMethod("sparse_mean", "sparse_numeric", function(x) {
  sum(x@value) / x@length
})

