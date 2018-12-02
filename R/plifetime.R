#' @title Probability of Battery Lifetime
#'
#' @description Let X & Y be the survival times of two batteries. This functions
#' calculates the probability that (\emph{X, Y}) falls within the rectangle which
#' represents the distribution of battery lifetime.The modes of the survival time
#' are (x, y) = (sqrt(2)/2, sqrt(2)/2). X and Y denote the lifetimes of the batteries
#' of the component in standard units.
#'
#' @param x1 a continuous vertical line which intersects with lines 'c' and 'd'
#' @param x2 a continuous vertical line which intersects with lines 'c' and 'd'
#' @param y1 a continuous horizontal line which intersects with lines 'a' and 'b'
#' @param y2 a continuous horizontal line which intersects with lines 'a' and 'b'
#'
#' @details Below is the function's pdf:
#' ```
#' pdf(x,y) = \{ 4xye^-(x^2 + y^2), when x > 0, y > 0
#'            \{ 0                  otherwise
#' ```
#' This function is used to solve Exercise 2.1.6 on page 98 of the book.
#'
#' @return The probability that (\emph{X, Y}) falls within the rectangle, which is
#' created at the intersections of the the lines 'a', 'b', 'c', and 'd'.
#'
#' @references Hogg, R., McKean, J., Craig, A. (2018) Introduction to Mathematical
#'             Statistics, 8th Ed. Boston: Pearson.
#'
#' @examples
#' x1 <- 2
#' x2 <- 3
#' y1 <- 2
#' y2 <- 3
#'
#' plifetime(x1, x2, y1, y2)
#'
#' # Probability of (X, Y) surviving beyond the mode lifetime of the batteries
#' x1 <- 1
#' x2 <- 2
#' y1 <- Inf
#' y2 <- Inf
#'
#' plifetime(x1, x2, y1, y2)
#'
#' @export plifetime

plifetime <- function(x1, x2, y1, y2) {

    # checking arguments
    errors <- makeAssertCollection()
    # argument 1 x1
    errors$push(has_nonan(x1, 1))
    errors$push(is_numeric(x1, 1))
    errors$push(is_oneelement(x1, 1))
    # argument 2 x2
    errors$push(has_nonan(x2, 2))
    errors$push(is_numeric(x2, 2))
    errors$push(is_oneelement(x2, 2))
    # argument 3 y1
    errors$push(has_nonan(y1, 3))
    errors$push(is_numeric(y1, 3))
    errors$push(is_oneelement(y1, 3))
    # argument 4 y2
    errors$push(has_nonan(y2, 4))
    errors$push(is_numeric(y2, 4))
    errors$push(is_oneelement(y2, 4))
    # argument check results
    reportAssertions(errors)

    # Function starting position
    (exp(-x1^2) - exp(-x2^2)) * (exp(-y1^2) - exp(-y2^2))
}

