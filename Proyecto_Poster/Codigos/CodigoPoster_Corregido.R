# 1. Obtener la longitud total de tu serie original
longitud_diff <- length(d.btts)
dummy_atipicos <- rep(0, longitud_diff)


# 3. Asignar un 1 en las posiciones 298 y 299
dummy_atipicos[c(297, 298)] <- 1

arima_0.0.1_corregido <- arima(d.btts, 
                               order = c(0, 0, 1), 
                               include.mean = FALSE, 
                               method = "ML", 
                               xreg = dummy_atipicos)
stargazer(arima_0.0.1_corregido,type="text",style = "aer")

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
res_arima_0.0.1_corregido = residuals(arima_0.0.1_corregido)
par(mfrow=c(1,2))

acf(res_arima_0.0.1_corregido,lag.max=24,plot=T,lwd=1,xlab='',
    main='ACF residuales (0,0,1)') 

pacf(res_arima_0.0.1_corregido,lag.max=24,plot=T,lwd=1,xlab='',
     main='ACF al cuadrado residuales (0,0,1)')
par(mfrow=c(1,1))

# Pruebas formales:

#~~ BOX-PIERCE TEST ~~# 

#ARIMA 0,0,1
Box.test(res_arima_0.0.1_corregido, lag=lags.test, type = "Box-Pierce") 
Box.test(res_arima_0.0.1_corregido, lag=10, type='Box-Pierce') 


#~~ LJUNG-BOX ~~#

#ARIMA 0,0,1
Box.test(res_arima_0.0.1_corregido, lag=lags.test, type = c("Ljung-Box")) 
Box.test(res_arima_0.0.1_corregido , lag=10, type='Ljung-Box') 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### 3.2. Homocedasticidad de los residuales ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#Ho = Homocedasticidad 
#Ha = heterocedasticidad


arch_deuda_arima_0.0.1_corregido = arch.test(arima_0.0.1_corregido, output=TRUE)

#Si queremos obtener un unico p-value para un número de lags
#en especifico se puede utilizar: 

#Hallamos los residuos 
residuos_corregido <- residuals(arima_0.0.1_corregido)

#Realizamos la prueba

ArchTest(residuos_corregido, lags = 44.5)


#PRUEBA WHITE 

arima_0.0.1_d.btts_corregido <- auto.arima(d.btts)

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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### 3.3. Normalidad en los residuales ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#--> ARIMA(3,0,0)
x11()
qqPlot(res_arima_0.0.1_corregido, ylab = "ARMA(0,1)")


x11()
qqPlot(res_arima_0.0.1_corregido, ylab = "ARMA(0,1)")

#Vemos colas pesadas

# Prueba formal: Jarque-Bera Test

#Ho = Normalidad
#Ha = No hay normalidad


jarque.bera.test(res_arima_0.0.1_corregido) 

dummy_atipicos
