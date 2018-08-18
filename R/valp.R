#' Valor p en múltiples t.test 
#' 
#' Esta función genera una población de números aleatorios con
#' distribución normal y con base en ella se realizan múltiples
#' muestreos; luego se emplea el t.test para obtener el número de veces que
#' el contraste de hipótesis genera conclusiones erradas.
#' 
#' Detalles...
#' 
#' @param n tamaño poblacional (por defecto igual a 1000)
#' @param gp tamaño muestral (por defecto igual a 20)
#' @param cp número de comparaciones (por defecto igual a 100)
#' @param nc nivel de confianza (por defecto igual a 0.95)
#' 
#' @return un vector con los valores p obtenidos para las comparaciones
#' establecidas.
#' 
#' @export
valp <- function(n = 1000, gp = 20,  cp = 100, nc = 0.95){
  pob = rnorm(n = n)
  g1 = list()
  vp = c()
  for (i in 1:cp) {
    g1[[i]] = sample(pob, size = gp)
    vp[i] = t.test(x = g1[[i]], alternative = "two.sided", conf.level = nc,
                   mu = 0)$p.value
  }
  return(vp)
}

#' Gráfico de valores p en t.test
#' 
#' Gráfico de valores p obtenido con valp.
#' 
#' @param x vector de valores p con la función \code{\link{valp}}
#'
#' @export
#' @import ggplot2
plot.valp <- function(x) {
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
