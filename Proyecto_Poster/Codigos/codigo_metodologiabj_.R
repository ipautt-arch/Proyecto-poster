install.packages("here")

# Trabajar con rutas relativas en R
library(fs)
library(here)

# Paquetes del tidyverse (Para el manejo, manipulación y graficación de datos)
library(readr)
library(dplyr)
library(ggplot2)

install.packages("ggtime")
library(ggtime)

# Paquetes del tidyverts (Para un manejo moderno de series de tiempo en R)
library(tsibble)
install.packages("feasts")
library(feasts)
install.packages("feasts")
library(fable)

# Paquetes adicionales para trabajar con series de tiempo en R

install.packages("tseries")
library(tseries)
install.packages("FinTS")
library(FinTS)
library(lmtest)
library(urca) # Test de raíz unitaria
library(readr)

install.packages("patchwork")

# Para que la función ARIMA que se use por defecto sea la de fable.
ARIMA <- fable::ARIMA

# Limpiamos el entorono 

rm(list = ls())
dev.off()
#___________________________________________________________________________________#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#####  Instalación de Paquetes ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Descargamos e importamos los paquetes que vayamos a usar con el paquete "pacman"

library(pacman)

# Pacman contiene una función denominada "p_load" que permite al usuario descargar
# un paquete e importarlo si no lo tiene, y si el usuario tiene descargado el 
# paquete, Pacman lo importa automáticamente. Veamoslo. 

pacman::p_load(
  
  forecast,   # Para hacer pronósticos con modelos arima
  lmtest,     # Significancia individual de los coeficientes ARIMA
  urca,       # Prueba de raíz unitaria
  tseries,    # Para estimar modelos de series de tiempo y hacer pruebas de supuestos
  stargazer,  # Para presentar resultados más estéticos
  psych,      # Para hacer estadísticas descriptiva
  seasonal,   # Para desestacionalizar series
  aTSA,       # Para hacer la prueba de efectos ARCH
  astsa,      # Para estimar, validar y hacer pronósticos para modelos ARIMA/SARIMA
  xts,        # Para utilizar objetos xts 
  tidyverse,  # Conjunto de paquetes (incluye dplyr y ggplot2)
  readxl,     # Para leer archivos excel 
  car,        # Para usar la función qqPlot
  mFilter,    # Para aplicar el Filtro Hodrick-Prescott
  quantmod,    
  
  # Paquetes del tidyverts
  
  fable,      # Forma moderna de hacer pronóstiocs en R (se recomienda su uso)  
  tsibble,    # Para poder emplear objetos de series de tiempo tsibble
  feasts      # Provee una colección de herramientas para el análisis de datos de series de tiempo 
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                         METODOLOGÍA BOX-JENKINS                              #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### 1. Primer paso: Identificación ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Cargar bases de datos en R usando rutas relativas ---

# Fijar la ruta del archivo actual como referencia para here()

here::i_am("Codigos/codigo_metodologiabj_.R") 

# Obtener la ruta del directorio con los datos
directorio <- fs::path(here::here("HQMCB12YR.csv", "Datos"))

# Rutas de las bases de datos
ruta_exp <- fs::path(directorio, "HQMCB12YR.csv") # Base de datos de Tasas

# Funciones auxiliares ---

# Función auxiliar para mostrar gráficos en una grilla m x n

grilla <- function(..., nrow, ncol) {
  graficos <- list(...)
  
  if (length(graficos) > nrow * ncol) {
    stop("La cantidad de gráficos supera el tamaño de la grilla.")
  }
  
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow = nrow, ncol = ncol)))
  
  for (i in seq_along(graficos)) {
    fila <- ceiling(i / ncol)
    columna <- ((i - 1) %% ncol) + 1
    
    print(
      graficos[[i]],
      vp = grid::viewport(layout.pos.row = fila, layout.pos.col = columna)
    )
  }
  
  grid::popViewport()
}

# === Tasa al contado de bonos corporativos
# de mercado de alta calidad (HQM) a 12 años==== 

# Base de datos con la serie importada a R

library(readr)

datos <- read_csv("~/Proyecto_Poster/Datos/HQMCB12YR.csv", 
                  col_names = TRUE, 
                  show_col_types = FALSE)

# Ver el tipo de objeto de la base de datos (tibble/data.frame)
print(class(datos))

# Ver primeras y últimas observaciones de la base de datos
print(head(datos)) # Primeras observaciones
print(tail(datos)) # Últimas observaciones

# Creación de la serie de tiempo de "Tasas al contado" ---

serie_bon <- datos
colnames(serie_bon)[2] <- "prices"


#Las volvemos ts y xts

btts = ts(serie_bon$prices, start = 1984, frequency = 12)
btxts = xts(serie_bon$prices, 
            order.by = serie_bon$observation_date) 

t = as.vector(t(serie_bon$prices))
ts = ts(t[1:508], start = c(1984), frequency = 12)


#Graficamos la serie

plot(btxts, main = "Tasa al contado de bonos corporativos
# de mercado de alta calidad (HQM) a 12 años ",
     sub = "1984-2026",
     ylab  = "%")

#Graficamos las FAC y las FACP

lags <- 24

x11()

par(mfrow = c(1, 2))
acf(btts, lag.max = lags, plot = TRUE, lwd = 2, xlab = '', main = 'ACF', ylim = c(-1, 1)) 
pacf(btts, lag.max = lags, plot = TRUE, lwd = 2, xlab = '', main = 'PACF', ylim = c(-1, 1))

#Prueba D-F para la serie normal

resultado_adf <- adf.test(btts)
print(resultado_adf)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### Transformación para volver estacionaria la serie #### 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#  Aplicar diff() : 

d.btts= diff(btts) # Serie diferenciada

# Vamos a graficar ahora su nivel, su variación, su tasa de crecimiento y su 
# valor en logaritmos.

x11()
par(mfrow=c(2,2))

plot.ts(btts, xlab="",ylab="", 
        main="Serie Normal",lty=1, lwd=2, col="lightblue")
plot.ts(d.btts, xlab="",ylab="", 
        main="Variación de los bonos",lty=1, lwd=2, col="orange")

#Miramos las FAC y las FACP de la serie que escogimos 

lags <- 24
x11()
par(mfrow=c(1,2))

acf(d.btts, lag.max = lags, plot=T, lwd=2, xlab='', main='ACF', ylim=c(-1,1)) 
pacf(d.btts, lag.max = lags, plot=T, lwd=2, xlab='', main='PACF', ylim=c(-1,1))

par(mfrow=c(1,1))
#Prueba D-F para la serie diferencia

resultado_adf <- adf.test(d.btts)
print(resultado_adf)

#Se concluye que el proceso es estacionario

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### 1.Identificacion del Modelo-Criterios de informacion####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

AR.m <- 6 
MA.m <- 6 

#Esta linea de codigo sirve para mostrar opciones de procesos ARIMA

arma_seleccion_df = function(ts_object, AR.m, MA.m, d, bool_trend, metodo){
  
  index = 1
  df = data.frame(p = double(), d = double(), q = double(), AIC = double(), BIC = double())
  for (p in 0:AR.m) {
    for (q in 0:MA.m)  {
      fitp <- arima(ts_object, order = c(p, d, q), include.mean = bool_trend, 
                    method = metodo)
      df[index,] = c(p, d, q, AIC(fitp), BIC(fitp))
      index = index + 1
    }
  }  
  return(df)
}

arma_min_AIC = function(df){
  df2 = df %>% 
    filter(AIC == min(AIC))
  return(df2)
}


arma_min_BIC = function(df){
  df2 = df %>% 
    filter(BIC == min(BIC))
  return(df2)
}


mod_d1_bond = arma_seleccion_df(btts, AR.m, MA.m, d = 1, TRUE, "ML")

min_aic = arma_min_AIC(mod_d1_bond); min_aic
min_bic= arma_min_BIC(mod_d1_bond); min_bic

view(mod_d1_bond)

# Elegimos el modelo, ARIMA de orden (0,1,1)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### Estimación ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

arima_0.1.1 = arima(btts, order = c(0,1,1), include.mean = T, 
                    method = "ML")
# Ver estimaciones, errores estándar, z-stat y p-values de los coeficientes
lmtest::coeftest(arima_0.1.1)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### Verificacion de supuestos de supuestos ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### No autocorrelación de los errores ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# De lags usamos un cuarto de la muestra 

lags.test = length(btts)/4;lags.test


# Grafica de las autocorrelaciones

#ARIMA 0,0,1

x11()
res_arima_0.1.1 = residuals(arima_0.1.1)
par(mfrow=c(1,2))

acf(res_arima_0.1.1, lag.max = lags, plot=T, lwd=2, xlab='', main='ACF', ylim=c(-1,1)) 
pacf(res_arima_0.1.1, lag.max = lags, plot=T, lwd=2, xlab='', main='PACF', ylim=c(-1,1))

