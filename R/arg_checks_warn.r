
is_positive <- function(x, argpos, msg = NULL) {
  # check to see if argument is a positive number 0 is not positive
  if (is.numeric(x)) {
    if (x >= 0) {
      return()
    }
  }
  if (!is.null(msg)) {
    return(msg)
  }
  v <- c("argument", argpos, "must be positive")
  return(paste(v, collapse = " "))
}

is_negative <- function(x, argpos) {
  # check to see if argument is a negative number 0 is not negitive
  if (is.numeric(x)) {
    if (x <= 0) {
      return()
    }
  }
  v <- c("argument", argpos, "must be negative")
  return(paste(v, collapse = " "))
}

is_nonzero <- function(x, argpos, msg = NULL) {
  # check to see if argument is not 0
  if (is.numeric(x)) {
    if (x != 0) {
      return()
    }
  }
  if (!is.null(msg)) {
    return(msg)
  }
  v <- c("argument", argpos, "must be numeric and non-zero")
  return(paste(v, collapse = " "))
}

is_logical <- function(x, argpos) {
  # check to see if argument is a logical
  if (is.logical(x)) {
    return()
  }
  v <- c("argument", argpos, "must be logical")
  return(paste(v, collapse = " "))
}


is_integer <- function(x, argpos, msg = NULL) {
  # check to see if argument is an integer
  if (is.numeric(x)) {
    if (is.nan(x%%1) || x%%1 == 0) {
      return()
    }
  }
  if (!is.null(msg)) {
    return(msg)
  }
  v <- c("argument", argpos, "must be an integer")
  return(paste(v, collapse = " "))
}

is_oneelement <- function(x, argpos) {
  if (length(x) == 1) {
    return()
  }
  v <- c("argument", argpos, "cannot have length greater than 1")
  return(paste(v, collapse = " "))
}

is_manyelement <- function(x, argpos) {
  if (length(x) > 1) {
    return()
  }
  v <- c("argument", argpos, "must have length greater than 1")
  return(paste(v, collapse = " "))
}

is_numvector <- function(x, argpos) {
  if (is.vector(x, mode = "numeric")) {
    return()
  }
  v <- c("argument", argpos, "must be a numeric vector")
  return(paste(v, collapse = " "))
}

is_numeric <- function(x, argpos, msg = NULL) {
  # check to see if argument is a number
  if (is.numeric(x)) {
    return()
  }
  if (!is.null(msg)) {
    return(msg)
  }
  v <- c("argument", argpos, "must be a number")
  return(paste(v, collapse = " "))
}

is_smaller <- function(x, y, argposx, argposy) {
  # check to see if on x < y
  if (x < y) {
    return()
  }
  v <- c("argument", argposx, "must be smaller than argument", argposy)
  return(paste(v, collapse = " "))
}

is_inrange <- function(x, argpos, min, max, msg = NULL) {
  # only works for single values
  if (length(x) > 1) {
    v <- c("argument", argpos, "cannot have a length greater than 1")
    return(paste(v, collapse = " "))
  }
  if (x >= min && x <= max) {
    return()
  }
  if (!is.null(msg)) {
    return(msg)
  }
  v <- c("argument", argpos, "must be greater than or equal to", min, "and less than or equal to",
    max)
  return(paste(v, collapse = " "))
}

is_xrange <- function(x, argpos, min, max, msg = NULL) {
  # only works for single values
  if (length(x) > 1) {
    v <- c("argument", argpos, "cannot have a length greater than 1")
    return(paste(v, collapse = " "))
  }
  if (x > min && x < max) {
    return()
  }
  if (!is.null(msg)) {
    return(msg)
  }
  v <- c("argument", argpos, "must be greater than", min, "and less than", max)
  return(paste(v, collapse = " "))
}

is_vecinrange <- function(x, argpos, min, max) {
  outrange <- 0
  for (i in 1:length(x)) {
    if (is.nan(x[i])) {

    } else if (x[i] < min || x[i] > max) {
      outrange <- outrange + 1
    }
  }
  if (outrange > 0) {
    v <- c("all elements in argument", argpos, "must be greater than or equal to", min, "and less than or equal to", 
      max)
    return(paste(v, collapse = " "))
  } else {
    return()
  }
}

is_vecxrange <- function(x, argpos, min, max) {
  outrange <- 0
  for (i in 1:length(x)) {
    if (is.nan(x[i])) {

    } else if (x[i] <= min || x[i] >= max) {
      outrange <- outrange + 1
    }
  }
  if (outrange > 0) {
    v <- c("all elements in argument", argpos, "must be greater than", min, "and less than", 
      max)
    return(paste(v, collapse = " "))
  } else {
    return()
  }
}

has_elements <- function(x, argpos, elements) {
  if (length(x) == elements) {
    return()
  }
  v <- c("argument", argpos, "must have", elements, "elements")
  return(paste(v, collapse = " "))
}

is_noninf <- function(x, argpos) {
  if (!is.infinite(x)) {
    return()
  }
  v <- c("argument", argpos, "cannot be infinite")
  return(paste(v, collapse = " "))
}

has_noinf <- function(x, argpos) {
  for (i in 1:length(x)) {
    if (is.infinite(x[i])) {
      v <- c("argument", argpos, "cannot include an Inf or -Inf")
      return(paste(v, collapse = " "))
    }
  }
  return()
}

has_nonan <- function(x, argpos) {
  for (i in 1:length(x)) {
    if (is.nan(x[i])) {
      v <- c("argument", argpos, "cannot include a NaN")
      return(paste(v, collapse = " "))
    }
  }
  return()
}

is_matrix <- function(x, argpos) {
  if (is.matrix(x)) {
    return()
  }
  v <- c("argument", argpos, "must be a matrix")
  return(paste(v, collapse = " "))
}

is_posmatrix2 <- function(x, argpos) {
  # is a all positive entry 2x2 matrix must be a 2x2 matrix
  if (x[1, 1] >= 0 && x[1, 2] >= 0 && x[2, 1] >= 0 && x[2, 2] >= 0) {
    return()
  }
  v <- c("argument", argpos, "must have all positive entries")
  return(paste(v, collapse = " "))
}

is_posdetmat2 <- function(x, argpos) {
  # is positive determinate 2x2 matrix
  if (is.matrix(x)) {
    if (x[1, 1] * x[2, 2] - x[1, 2] * x[2, 1] > 0) {
      return()
    }
    v <- c("the determinate of argument", argpos, "must be positive")
    return(paste(v, collapse = " "))
  }
  v <- c("argument", argpos, "must be a matrix")
  return(paste(v, collapse = " "))
}

is_linearmodel <- function(x, argpos) {
  if (class(x) == "lm") {
    return()
  }

  v <- c("argument", argpos, "must be a linear model")
  return(paste(v, collapse = " "))
}
