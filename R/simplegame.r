#' @title Simple Game for Estimation of Expected Gain
#' @description Computes the estimation of expected gain for a game wherein the following rules apply:
#' A player pays p_0 to play. The player then rolls a 6-sided die. If the roll is a 1 or a 2 then the game is over.
#' Otherwise, the player flips a fair coin. If the coin toss results in a tail, the player receives $1 and the game is over.
#' Otherwise they draw 2 cards without replacement from the standard deck of 52 cards. If none of the cards is an ace
#' they receive $2, while they receive $10 or $50 if they get 1 or 2 aces respectively. In both cases, the game is over.
#' Let G denote the player's gain. To determine the expected gain, we need the distribution of G. The support of G is
#' the set {-p_0, 1-p_0, 2-p_0, 10-p_0, 50-p_0}. For the associated probabilities we need the distribution of X where
#' X is the number of aces in a draw of 2 cards. The distribution is
#' \eqn{P(X=x) = ( (4 choose x) * (48 choose (2-x)) ) / (52 choose 2) for x=0,1,2}
#'
#' See example 1.8.9 on page 65 of the book.
#'
#' @param amtpaid Initial amount that a player pays (p_0 in description).
#'
#' @return Estimation of expected gain for player.
#'
#' @references Hogg, R. McKean, J. Craig, A (2018) Introduction to Mathematical Statistics, 8th Ed. Boston: Pearson
#'
#' @examples
#' player_winnings <- simplegame(1)
#' player_winnings <- simplegame(5)
#' player_winnings <- simplegame(10)
#'
#' @export simplegame

simplegame <- function(amtpaid) {
  
  # INPUT VALIDATION
  errors <- makeAssertCollection()
  # argument 1: amtpaid
  errors$push(has_nonan(amtpaid, 1))
  reportAssertions(errors)
  
  errors$push(is_numeric(amtpaid, 1))
  errors$push(is_nonzero(amtpaid, 1))
  errors$push(is_positive(amtpaid, 1))
  errors$push(has_noinf(amtpaid, 1))
  reportAssertions(errors)
  
  # FUNCTION BEGINS
  
  gain <- -amtpaid
  x <- 0:2  # vector of integers 0, 1, 2
  pace <- (choose(4, x) * choose(48, 2 - x))/choose(52, 2)
  x <- sample(1:6, 1, prob = rep(1/6, 6))
  if (x > 2) {
    y <- sample(0:1, 1, prob = rep(1/2, 2))
    if (y == 0) {
      gain <- gain + 1
    } else {
      z <- sample(0:2, 1, prob = pace)
      if (z == 0) {
        gain <- gain + 2
      } else if (z == 1) {
        gain <- gain + 10
      } else if (z == 2) {
        gain <- gain + 50
      }
    }
  }
  return(gain)
}
