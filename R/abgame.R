#' @title Coin Flip versus Die Roll Game
#'
#' @description This function simulates a game involving two players. Person A
#' flips a fair coin initially, with the objective of getting a head. Person B
#' rolls a fair six-sided die after Person A\'s move with the obejective of getting
#' a 1, 2, 3, or 4. The game is repeated until one person is able to complete his
#' or her objective first thereby winning the game. The winner of the game
#' receives a 1 while the loser receives a 0.
#'
#' @details The function plays one round of the game referenced in
#' Example 1.4.8 (8th Edition). There is no input required to run this simulation.
#' The function defines all possible outcomes of a coin toss or the rolling of a
#' die, as well as calculates the probability of both the coin landing on heads,
#' and the die landing on one through four.
#'
#' @return \code{results} contains the resulting score of one round of the game.
#'
#' @references Hogg, R., McKean, J., Craig, A. (2018) Introduction to Mathematical
#'             Statistics, 8th Ed. Boston: Pearson.
#'
#' @examples
#' ind <- 0
#' nsims <- 10000
#' for (i in 1:nsims) {
#'     seeA <- abgame()
#'     if (seeA[1] == 1){
#'         ind <- ind + 1
#'     }
#' }
#' estpA <- ind/nsims
#' err <- 1.96 * sqrt(estpA * (1 - estpA)/ nsims)
#' estpA; err
#'
#' @export abgame

abgame <- function() {
  # function starts define possible outcomes for flipping a coin and rolling a dice
  rngA <- c(0, 1)
  rngB <- 1:6
  # defines probability of outcomes for flipping a coin and rolling dice
  pA <- rep(1/2, 2)
  pB <- rep(1/6, 6)
  # counter for while loop
  ic <- 0
  # keep score for coin flipping and rolling dice in these variables
  Awin <- 0
  Bwin <- 0
  # output of each loop is the result of 1 game played
  while (ic == 0) {
    # draws a sample size 1 from rngA
    x <- sample(rngA, 1, prob = pA)
    # if x is one a wins
    if (x == 1) {
      ic <- 1
      Awin <- 1
    } else {
      # draws a sample size 1 from rngB
      y <- sample(rngB, 1, prob = pB)
      # if y is less than or equal to 4 b wins
      if (y <= 4) {
        ic <- 1
        Bwin <- 1
      }
    }
  }
  # combine results
  results <- c(Awin, Bwin)
  names(results) <- c("Awin", "Bwin")
  # report results
  return(results)
}
