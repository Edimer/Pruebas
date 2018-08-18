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
#' @examples
#' valp(n = 100, gp = 5, cp = 10, nc = 0.90)
#' valp(n = 100, gp = 10, cp = 10, nc = 0.95)
#' valp(n = 100, gp = 15, cp = 10, nc = 0.99)
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