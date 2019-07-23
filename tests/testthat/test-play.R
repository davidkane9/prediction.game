test_that("bad arguments produce errors", {
  expect_error(play(n = "T"))
  expect_error(play(n = 1, guess_1 = "T"))
  expect_error(play(n = 1, guess_1 = 10, guess_2 = "T"))
  expect_error(play(n = 1, guess_1 = 10, guess_2 = 11, formula = mean(mtcars$mpg)))
})