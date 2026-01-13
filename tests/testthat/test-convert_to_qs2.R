test_that("convert_to_qs2 funciona correctamente", {
  # Setup: crear datos de prueba
  df_ejemplo <- data.frame(a = 1:10, b = letters[1:10])
  csv_file <- tempfile(fileext = ".csv")
  utils::write.csv(df_ejemplo, csv_file, row.names = FALSE)

  # 1. Test: guardar desde objeto (debe fallar sin nombre)
  expect_error(convert_to_qs2(df_ejemplo))

  # 2. Test: guardar desde objeto con nombre
  qs2_file_obj <- tempfile(fileext = ".qs2")
  convert_to_qs2(df_ejemplo, qs2_file_obj)
  expect_true(file.exists(qs2_file_obj))

  # 3. Test: guardar desde CSV (auto-nombre)
  res_path <- convert_to_qs2(csv_file)
  expect_true(file.exists(res_path))
  expect_equal(tools::file_ext(res_path), "qs2")

  # Limpieza automática: tempfile se borra solo al cerrar R,
  # pero podés borrar los creados manualmente si querés.
})
