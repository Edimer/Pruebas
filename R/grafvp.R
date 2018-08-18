#' Gráfico de valores p en t.test
#' 
#' Gráfico de valores p obtenido con valp.
#' 
#' @param x vector de valores p con la función \code{\link{valp}}
#'
#' @examples
#' grafvp(valp()) # 100 comparaciones por defecto
#' grafvp(valp(cp = 10)) # modificando el número de comparaciones
#'
#' @importFrom stats rnorm t.test
#'
#' @export
#' @import ggplot2
grafvp <- function(x) {
  df = data.frame(index = c(1:length(x)), valorP = x)
  g = ggplot(data = df, aes(x = index, y = valorP)) +
    geom_point() +
    geom_hline(yintercept = 0.01, lwd = 0.5, color = "firebrick", lty = 2) +
    geom_hline(yintercept = 0.05, lwd = 0.5, color = "dodgerblue4", lty = 2) +
    geom_hline(yintercept = 0.10, lwd = 0.5, color = "green4", lty = 2) +
    labs(x = "", y = "Valor p",
         title = paste0("Valores p para ", length(x), " comparaciones",
                        sep = ""),
         caption = "Línea roja: 0.01\nLínea azul: 0.05\nLínea verde: 0.10") +
    theme_linedraw()
  return(g)
}