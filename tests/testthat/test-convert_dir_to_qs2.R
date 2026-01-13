test_that("convert_dir_to_qs2 funciona correctamente", {
  # 1. Preparar un directorio temporal con archivos de prueba
  test_dir <- file.path(tempdir(), "test_folder")
  dir.create(test_dir)

  # Crear un CSV y un QS de prueba
  df <- data.frame(x = 1:5, y = letters[1:5])
  utils::write.csv(df, file.path(test_dir, "data1.csv"), row.names = FALSE)
  qs::qsave(df, file.path(test_dir, "data2.qs"))

  # 2. Ejecutar la función de directorio
  res <- convert_dir_to_qs2(test_dir)

  # 3. Verificaciones (Expectations)
  expect_length(res, 2) # Debería haber devuelto 2 rutas
  expect_true(file.exists(file.path(test_dir, "data1.qs2")))
  expect_true(file.exists(file.path(test_dir, "data2.qs2")))

  # 4. Test de carpeta vacía o sin archivos compatibles
  empty_dir <- file.path(tempdir(), "empty_folder")
  dir.create(empty_dir)
  expect_message(convert_dir_to_qs2(empty_dir), "No se encontraron archivos")

  # Limpieza
  unlink(test_dir, recursive = TRUE)
  unlink(empty_dir, recursive = TRUE)
})
