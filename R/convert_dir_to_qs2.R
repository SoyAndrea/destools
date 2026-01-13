#' Convertir un directorio entero a formato qs2
#'
#' Busca todos los archivos .csv y .qs en una carpeta y los convierte a .qs2.
#'
#' @param path Ruta a la carpeta que contiene los archivos.
#' @param recursive Logico. ¿Debe buscar tambien en subcarpetas?
#'
#' @export
convert_dir_to_qs2 <- function(path = ".", recursive = FALSE) {

  # 1. Listar archivos .csv y .qs
  # Usamos un regex para detectar ambas extensiones
  pattern <- "\\.(csv|qs)$"
  archivos <- list.files(path = path,
                         pattern = pattern,
                         full.names = TRUE,
                         recursive = recursive)

  if (length(archivos) == 0) {
    message("No se encontraron archivos .csv o .qs en: ", path)
    return(invisible(NULL))
  }

  message("Se encontraron ", length(archivos), " archivos. Iniciando conversion...")

  # 2. Aplicar nuestra funcion convert_to_qs2 a cada archivo
  # Usamos lapply para iterar sobre la lista
  resultados <- lapply(archivos, function(f) {
    tryCatch({
      # Llamamos a la funcion que ya teniamos
      destools::convert_to_qs2(f)
    }, error = function(e) {
      warning("No se pudo convertir: ", f, " - Error: ", e$message)
      return(NULL)
    })
  })

  message("--- Proceso de directorio finalizado ---")
  return(invisible(unlist(resultados)))
}
