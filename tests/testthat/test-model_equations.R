test_that("Non-numeric inputs for auxiliary model equations wang give an error", {

  expect_error(wang(temp = 10, k = 25, r = "0.025", topt = 25, tmin = 10, tmax = 32, a = 0.025),
               "Non-numeric inputs for `wang` model. Model discarded.",
               fixed = TRUE)
})

test_that("Non-numeric inputs for auxiliary model equations mod_polynomial give an error", {

  expect_error(mod_polynomial(temp = 10, a_0 = 1, a_1 = 2, a_2 = 3, a_3 = 4, a_4 = as.factor(5)),
               "Non-numeric inputs for `mod_polynomial` model. Model discarded.",
               fixed = TRUE)
})


