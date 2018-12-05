# Test for mcpbon function Returns: An error message if an tests
# fail

context("mcpbon")

# Load data set
load(url("http://www.stat.wmich.edu/mckean/hmchomepage/Data/fastcars.rda"))

# Declare variables
speed <- fastcars$speed
car <- fastcars$ind

# Declare test variables
dummy_na <- replace(speed, 157.7, NA)
dummy_inf <- speed
dummy_inf[1] <- Inf

car_na <- car
car_na[1] <- NA

test_that("edge cases", {
  expect_error(mcpbon(dummy_inf, car), "argument 1 cannot include an Inf or -Inf")
  expect_error(mcpbon(speed, car, alpha = 1.1), "input object 'alpha' must be between 0 and 1")
  expect_error(mcpbon(speed, car, alpha = -1), "argument 3 must be positive")
})

test_that("input", {
  expect_error(mcpbon(speed, car, alpha = NA), "argument 3 must be numeric and non-zero")
  expect_error(mcpbon(speed, car, alpha = NA), "argument 3 must be positive")
  expect_error(mcpbon(speed, car, alpha = NA), "argument 3 must be a number")
})

test_that("output", {
  expect_equal(names(mcpbon(speed, car)), c("j", "jp", "muj", "mujp", 
    "diff", "se", "err", "lb", "ub"))
  expect_equal(length(mcpbon(speed, car)$j), 10)
  expect_equal(length(mcpbon(speed, car)$jp), 10)
  expect_equal(is.numeric(mcpbon(speed, car)$muj), TRUE)
  expect_equal(is.numeric(mcpbon(speed, car)$mujp), TRUE)
  expect_equal(is.numeric(mcpbon(speed, car)$diff), TRUE)
  expect_equal(is.numeric(mcpbon(speed, car)$se), TRUE)
  expect_equal(is.numeric(mcpbon(speed, car)$err), TRUE)
  expect_equal(is.numeric(mcpbon(speed, car)$lb), TRUE)
  expect_equal(is.numeric(mcpbon(speed, car)$ub), TRUE)
})


