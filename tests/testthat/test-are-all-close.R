#1) correctly returns TRUE
test_that("correctly returns TRUE", {
  v = rep(1,10)
  u = rep(1,10)
  expect_true(are_all_close(
    v, u, abs_tol = 1e-2, rel_tol = 1e-2
  ))
})

#2) correctly returns FALSE
test_that("correctly returns FALSE because the relative error is above rel_tol", {
  set.seed(1)
  v = rnorm(1)
  u = v+0.1
  expect_false(are_all_close(
    v, u, abs_tol = 1e-2, rel_tol = 1e-16
  ))
})


#3) correctly returns FALSE
test_that("correctly returns FALSE because the absolute error is above abs_tol", {
  set.seed(1)
  v = rnorm(1)
  u = rnorm(1)+10
  expect_false(are_all_close(
    v, u, abs_tol = 1e-6, rel_tol = 1e-6
  ))
})
