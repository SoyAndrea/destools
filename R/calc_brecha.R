#' Calcular brecha porcentual
#'
#' Esta función calcula la diferencia porcentual entre dos valores.
#' Es la fórmula estándar de la DES para medir brechas.
#'
#' @param valor1 El valor del grupo de interés (ej. Mujeres).
#' @param valor2 El valor de referencia o base (ej. Varones).
#'
#' @return Un valor numérico que representa la brecha porcentual.
#' @export
#'
#' @examples
#' calc_brecha(80, 100)
calc_brecha <- function(valor1, valor2) {
  if (valor2 == 0) stop("El valor2 no puede ser cero.")

  resultado <- ((valor1 / valor2) - 1) * 100
  return(round(resultado, 1))
}
