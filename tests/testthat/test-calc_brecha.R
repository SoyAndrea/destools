test_that("calc_brecha calcula correctamente y redondea a 1 decimal", {
  # Caso base: 80 vs 100 es -20%
  expect_equal(calc_brecha(80, 100), -20)

  # Caso con decimales: debe redondear a 1 solo
  # (105 / 100.35) - 1 * 100 = 4.633... -> debe ser 4.6
  expect_equal(calc_brecha(105, 100.35), 4.6)

  # Caso positivo
  expect_equal(calc_brecha(150, 100), 50)
})

test_that("calc_brecha arroja error si el valor2 es cero", {
  expect_error(calc_brecha(100, 0))
})
