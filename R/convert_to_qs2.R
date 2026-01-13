#' Convertir a formato qs2
#'
#' Lee un archivo (.csv o .qs) o recibe un data.frame y lo guarda como .qs2
#' usando el motor de qs2 para maxima eficiencia.
#'
#' @param x Un objeto (data.frame) o una ruta (string) a un archivo .csv o .qs.
#' @param output_file Ruta de destino. Si es NULL, se genera automaticamente para archivos.
#'
#' @export
convert_to_qs2 <- function(x, output_file = NULL) {

  if (is.character(x) && length(x) == 1) {
    if (!file.exists(x)) {
      stop("El archivo no existe: ", x) # 'existe' no tiene tilde, esta ok
    }

    if (is.null(output_file)) {
      name <- tools::file_path_sans_ext(x)
      output_file <- paste0(name, ".qs2")
    }

    ext <- tools::file_ext(x)
    data_obj <- switch(ext,
                       "qs"  = qs::qread(x),
                       "csv" = utils::read.csv(x),
                       stop("Extension no soportada. Solo .csv o .qs") # Quitamos tilde
    )

  } else if (is.data.frame(x)) {
    if (is.null(output_file)) {
      stop("Para objetos del environment, debes especificar un 'output_file'") # Quitamos tilde
    }
    data_obj <- x

  } else {
    stop("El argumento 'x' debe ser una ruta de archivo o un data.frame.") # 'debe' ok
  }

  qs2::qd_save(data_obj, output_file)

  message("Proceso completado. Archivo en: ", output_file) # 'completado' ok
  return(invisible(output_file))
}
