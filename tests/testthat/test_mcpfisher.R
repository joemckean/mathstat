# Test for mcpfisher function
#
# Returns:
#    An error message if an tests fail

context("mcpfisher")

# Load data set
load(url('http://www.stat.wmich.edu/mckean/hmchomepage/Data/fastcars.rda'))

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
    expect_error(mcpfisher(dummy_inf, car),
                 "argument 1 cannot include an Inf or -Inf")
    expect_error(mcpfisher(speed, car, alpha = 1.1),
                 "input object 'alpha' must be between 0 and 1")
    expect_error(mcpfisher(speed, car, alpha = -1),
                 "argument 3 must be positive")
})

test_that("input", {
    expect_error(mcpfisher(speed, car, alpha = NA),
                 "argument 3 must be numeric and non-zero")
    expect_error(mcpfisher(speed, car, alpha = NA),
                 "argument 3 must be positive")
    expect_error(mcpfisher(speed, car, alpha = NA),
                 "argument 3 must be a number")
})

test_that("output", {
    expect_equal(names(mcpfisher(speed, car)),
                 c("ftest", "tab"))
    expect_equal(length(mcpfisher(speed, car)$ftest), 2)
    expect_equal(length(mcpfisher(speed, car)$tab), 9)
    expect_equal(length(mcpfisher(speed, car)$tab$j), 10)
    expect_equal(length(mcpfisher(speed, car)$tab$jp), 10)
    expect_equal(length(mcpfisher(speed, car)$ftest), 2)
    expect_equal(is.numeric(mcpfisher(speed, car)$ftest), TRUE)
    expect_equal(is.numeric(mcpfisher(speed, car)$tab$muj), TRUE)
    expect_equal(is.numeric(mcpfisher(speed, car)$tab$mujp), TRUE)
    expect_equal(is.numeric(mcpfisher(speed, car)$tab$diff), TRUE)
    expect_equal(is.numeric(mcpfisher(speed, car)$tab$se), TRUE)
    expect_equal(is.numeric(mcpfisher(speed, car)$tab$err), TRUE)
    expect_equal(is.numeric(mcpfisher(speed, car)$tab$lb), TRUE)
    expect_equal(is.numeric(mcpfisher(speed, car)$tab$ub), TRUE)
})


