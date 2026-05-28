rm(list = ls())
dev.off()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### Librerias ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

library(pacman)


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
  sandwich,
  FinTS,
  # Paquetes del tidyverts
  
  fable,      # Forma moderna de hacer pronóstiocs en R (se recomienda su uso)  
  tsibble,    # Para poder emplear objetos de series de tiempo tsibble
  feasts      # Provee una colección de herramientas para el análisis de datos de series de tiempo 
)

base_prueba <- HQMCB12YR
colnames(base_prueba)[2] <- "prices"


#Las volvemos ts y xts

btts = ts(base_prueba$prices, start = 1984, frequency = 12)
btxts = xts(base_prueba$prices, 
                order.by = base_prueba$observation_date) 

t = as.vector(t(base_prueba$prices))
ts = ts(t[1:508], start = c(1984), frequency = 12)



tail(cbind(time(ts), covid), 40)

#Graficamos la serie

plot(btxts, main = "China",
     sub = "1975-2026",
     ylab  = "%")



#Graficamos las FAC y las FACP

lags=24

x11()
par(mfrow=c(1,2))
acf(btts, lag.max = lags, plot=T, lwd=2,xlab='',main='ACF') 
pacf(btts,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF')
par(mfrow=c(1,1))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### 1.3. Transformación para volver estacionaria la serie #### 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#  Aplicar diff() : 

d.btts= diff(btts) # Serie diferenciada

#  Aplicar log(): 

l.btts=log(btts) # Serie que se le aplica solo el logaritmo 


# Aplicar diff(log(serie_original)): 

dl.btts= diff(log(btts))*100   # Diferencia de logaritmos de la serie 
# (tasa de crecimiento)

# IMPORTANTE: Primero se aplica log y luego diff si van a usar ambos. 

# Vamos a graficar ahora su nivel, su variación, su tasa de crecimiento y su 
# valor en logaritmos.

x11()
par(mfrow=c(2,2))

plot.ts(btts, xlab="",ylab="", 
        main="M1 Real",lty=1, lwd=2, col="lightblue")
plot.ts(l.btts, xlab="",ylab="", 
        main="M1 Real en logaritmo",lty=1, lwd=2, col="black")
plot.ts(d.btts, xlab="",ylab="", 
        main="Variación M1 Real",lty=1, lwd=2, col="orange")
plot.ts(dl.btts, xlab="",ylab="",
        main="Tasa de crecimiento M1 Real",lty=1, 
        lwd=2, col="lightgreen")

#Miramos las FAC y las FACP de la serie que escogimos 

x11()
lags=30
par(mfrow=c(1,2))
acf(d.btts,lag.max=lags,plot=T,lwd=2,xlab='',
    main='ACF de la diff') 
pacf(d.btts,lag.max=lags,plot=T,lwd=2,xlab='',
     main='PACF de la diff')


resultado_adf <- adf.test(d.btts)
print(resultado_adf)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### 1.Identificacion del Modelo-Criterios de informacion####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

AR.m <- 6 
MA.m <- 6 

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



mod_d0_d.btts = arma_seleccion_df(d.btts, AR.m, MA.m, d = 0, TRUE, "ML")

min_aic = arma_min_AIC(mod_d0_d.btts ); min_aic
min_bic= arma_min_BIC(mod_d0_d.btts ); min_bic
view(mod_d0_d.btts)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### 2. Estimación ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

arima_0.1.1 = arima(btts, order = c(0,1,1), include.mean = T, 
                    method = "ML")


arima_0.0.1 = arima(d.btts, order = c(0,0,1), include.mean = F, 
                    method = "ML")

arima_1.0.1 = arima(d.btts, order = c(1,0,1), include.mean = F, 
                    method = "ML")


arima_2.0.1 = arima(d.btts, order = c(2,0,1), include.mean = F, 
                    method = "ML")


stargazer(arima_0.1.1,arima_0.0.1,arima_1.0.1,arima_2.0.1,type="text",style = "aer")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### 3. Verificacion de supuestos de supuestos ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### 3.1. No autocorrelación de los errores ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# De lags usamos un cuarto de la muestra 

lags.test = length(btts)/4;lags.test


# Grafica de las autocorrelaciones

#ARIMA 0,0,1

x11()
res_arima_0.0.1 = residuals(arima_0.0.1)
par(mfrow=c(1,2))

acf(res_arima_0.0.1,lag.max=24,plot=T,lwd=1,xlab='',
    main='ACF residuales (0,0,1)') 

pacf(res_arima_0.0.1,lag.max=24,plot=T,lwd=1,xlab='',
     main='ACF al cuadrado residuales (0,0,1)')
par(mfrow=c(1,1))


# ARIMA 3,1,0
res_arima_3.1.0 = residuals(arima_3.1.0)
par(mfrow=c(1,2))

acf(res_arima_3.1.0,lag.max=24,plot=T,lwd=1,xlab='',
    main='ACF residuales (3,1,0)') 

pacf(res_arima_3.1.0,lag.max=24,plot=T,lwd=1,xlab='',
     main='ACF al cuadrado residuales (3,1,0)')
par(mfrow=c(1,1))



# Pruebas formales:

#~~ BOX-PIERCE TEST ~~# 

#ARIMA 0,0,1
Box.test(res_arima_0.0.1, lag=lags.test, type = "Box-Pierce") 
Box.test(res_arima_0.0.1, lag=10, type='Box-Pierce') 


#~~ LJUNG-BOX ~~#

#ARIMA 0,0,1
Box.test(res_arima_0.0.1, lag=lags.test, type = c("Ljung-Box")) 
Box.test(res_arima_0.0.1 , lag=10, type='Ljung-Box') 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### 3.2. Homocedasticidad de los residuales ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#Ho = Homocedasticidad 
#Ha = heterocedasticidad


arch_deuda_arima_0.0.1 = arch.test(arima_0.0.1, output=TRUE)

#Si queremos obtener un unico p-value para un número de lags
#en especifico se puede utilizar: 

#Hallamos los residuos 
residuos <- residuals(arima_0.0.1)

#Realizamos la prueba

ArchTest(residuos, lags = 44.5)


#PRUEBA WHITE 

arima_0.0.1_d.btts <- auto.arima(d.btts)

# Obtener los residuos del modelo ARIMA
residuos <- residuals(arima_0.0.1)

# Realizar la prueba de White
white_test <- bptest(residuos ~ fitted(arima_0.0.1))

# Ver los resultados de la prueba

print(white_test)



# Grafica de los residuos al cuadrado

x11()
par(mfrow=c(1,2))
acf(res_arima_0.0.1^2,lag.max=lags,plot=T,lwd=2,xlab='',main='ACF residuales al cuadrado') 
pacf(res_arima_0.0.1^2,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF residuales al cuadrado')
par(mfrow=c(1,1))
