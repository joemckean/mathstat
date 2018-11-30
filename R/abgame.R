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

    rngA <- c(0, 1) # possible outputs for flipping a coin
    rngB <- 1:6 # possible outputs for rolling a dice

    pA <- rep(1/ 2, 2) # defines probability of outcomes for the coin flip
    pB <- rep(1/ 6, 6) # defines probability of outcomes for rolling a dice

    ic <- 0 # counter for while loop

    Awin <- 0 # score keeping for flipping a coin
    Bwin <- 0 # score keeping for rolling a dice

    while(ic == 0) { # output of each loop is the result of 1 game played
        x <- sample(rngA, 1, prob=pA) # draws a sample size 1 from rngA

        if(x == 1) {
            ic <- 1
            Awin <- 1
        }
        else {
            y <- sample(rngB, 1, prob=pB) # draws a sample size 1 from rngB

            if(y <= 4) {
                ic <- 1
                Bwin <- 1
            }
        }
    }

    results <- c(Awin, Bwin)
    names(results) <- c("Awin", "Bwin")

    return(results)
}
