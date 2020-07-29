#Se instalan y cargan librerias
if(!require("pacman")) install.packages("pacman")
p_load("readxl","quantmod","tidyverse","reshape2","fBasics","PerformanceAnalytics","quadprog",
       "IntroCompFinR")

# Se arma una variable con los volumenes de las acciones a evaluar
Acciones <- c("^GSPC", "^FCHI", "^GDAXI", "HSI", "MXX", "^BVSP", "^IPSA")

# Se obtienen los datos de los valores accionarios
getSymbols(Acciones, src = "yahoo", from = "2012-01-01", to = "2017-12-31", periodicity = "monthly")

# Dado un error con la funcion get en leer el simbolo ^ en el nombre de los indices, fue creado
# otro vector con los nombres de los indices para que pueda ser reconocido por la funcion
acciones1 <- c("GSPC", "FCHI", "GDAXI", "HSI", "MXX", "BVSP", "IPSA")

# Obteniendo el precio de cierre para graficar retornos acumulados, 
list <- lapply(acciones1, function(x) Cl(get(x)))
precio.cierre <- do.call(merge, list)

# Calculamos retornos
retornos <- data.frame(apply(precio.cierre, 2, function(x) Delt(x, type = "log")), 
                       fecha = index(precio.cierre)) %>% 
  na.omit() 

# Calculamos retornos acumulados
acumulados <- data.frame(apply(retornos[1:7], 2, function(x) cumsum(x)), 
                         fecha = index(precio.cierre[-1]))

# Cambiamos la forma de los datos
reshape.acumulados <- melt(acumulados, id.vars = "fecha")

# Construir grafico reotrnos acumulados
g2 <- ggplot(reshape.acumulados) + geom_line(mapping = aes(fecha, value, color = variable), size=0.8)
g2 <- g2 + ggtitle("Retornos Acumulados",
                   subtitle="GSPC, FCHI, GDAXI, HSI, MXX, BVSP y IPSA")
g2 <- g2 + xlab("Fecha") + ylab("Retornos")
g2 <- g2 + scale_color_brewer("Tickers", palette="Dark2")
g2 <- g2 + theme(legend.position = "right", size = 10, face = "bold",
legend.background = element_rect(fill = "darkgray"), legend.key.size = unit(1.5, "cm"),
legend.key.width = unit(0.5,"cm") 
)

g2
