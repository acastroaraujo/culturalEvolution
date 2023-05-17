
test_that("Function factory works as expected", {
  expect_equal(formalArgs(recursion(q, a + q)), c("params", "init", "tn"))
})

env_learn <- recursion(
  var = x,
  expr = x + (1 - x)*P1 - x*P2
)

# the parameter list cannot contain an object named t
test_that("Errors", {
  expect_error(env_learn(list(t = 1, P1 = 0.1, P2 = 0.1)))
})

# the parameter list cannot contain an object named x
test_that("Errors", {
  expect_error(env_learn(list(x = 1, P1 = 0.1, P2 = 0.1)))
})

# Error: P1, P2 missing from the parameter list
test_that("Errors", {
  expect_error(env_learn(list(O = 0.1)))
})

# The expression must contain either the state variable or t.
test_that("Errors", {
  expect_error(recursion(m, expr = a + b))
})
