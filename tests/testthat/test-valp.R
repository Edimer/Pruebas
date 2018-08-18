library(Pruebas)
context("Longitud del vector resultante")
test_that("Prueba la longitud del vector", {
  expect_equal(length(valp()), 100)
})