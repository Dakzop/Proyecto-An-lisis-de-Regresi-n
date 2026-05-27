library(readxl)
library(ggplot2)
library(dplyr)
library(GGally)
library(moments)

datos <- read_xlsx("C:/Users/nelso/Downloads/Real estate valuation data set.xlsx") #Colocar aquí la ruta del archivo#


#Función para ver datos vacios (NA)#
sum(is.na(datos))

#Resumen estadístico de los datos#
summary(datos,digits = 7)
Asimetria <- apply(datos,2,skewness)
curtosis <- apply(datos,2,kurtosis)
print(Asimetria)
print(curtosis)

#Correlación con la variable respuesta#
vars_numericas <- sapply(datos, is.numeric)
matriz_cor <- cor(datos[, vars_numericas], use = "complete.obs")

cor_demanda <- matriz_cor[, "Y house price of unit area"]
cor_demanda <- cor_demanda[order(abs(cor_demanda), decreasing = TRUE)]
print(round(cor_demanda, 3))

variables <- c("X1 transaction date","X2 house age", "X3 distance to the nearest MRT station", 
                      "X4 number of convenience stores","X5 latitude","X6 longitude" , "Y house price of unit area")

datos_grafico <- datos[, variables]
colnames(datos_grafico) <- c("Fecha_Trans", "Edad_Casa", "Dist_MRT", "Num_Tiendas", "Lat", "Long", "Precio_Ping")

ggpairs(datos_grafico, lower = list(continuous = wrap("points", size = 0.9, alpha = 0.5)), 
        upper = list(continuous = wrap("cor", size = 3.5))) + 
  theme_bw() +
  theme(
    axis.text = element_text(size = 7),     
    strip.text = element_text(size = 8),    
    panel.grid.major = element_blank()      
  )


hist(datos$`Y house price of unit area`)

#PLOT DE LAS VARIABLES CON LAS VARIABLES RESPUESTA#

#Scatter plots#
plot(datos$`X1 transaction date`, datos$`Y house price of unit area`,
     main = "Evolución del Precio por Unidad de Área",
     xlab = "Año de Transacción (Decimal)", 
     ylab = "Precio de vivienda por Unidad de Área",
     pch = 19,           
     col = "darkblue",   
     cex = 0.6)
lines(lowess(datos$`X1 transaction date`, datos$`Y house price of unit area`), col = "red", lwd = 2)

plot(datos$`X2 house age`, datos$`Y house price of unit area`,
     main = "Evolución del Precio por Unidad de Área",
     xlab = "Antiguedad de la vivienda", 
     ylab = "Precio de vivienda por Unidad de Área",
     pch = 19,           
     col = "darkblue",   
     cex = 0.6)
lines(lowess(datos$`X2 house age`, datos$`Y house price of unit area`), col = "red", lwd = 2)

plot(datos$`X3 distance to the nearest MRT station`, datos$`Y house price of unit area`,
     main = "Evolución del Precio por Unidad de Área",
     xlab = "Distancia física hacia la estación de metro más cercana", 
     ylab = "Precio de vivienda por Unidad de Área",
     pch = 19,           
     col = "darkblue",   
     cex = 0.6)
lines(lowess(datos$`X3 distance to the nearest MRT station`, datos$`Y house price of unit area`), col = "red", lwd = 2)

plot(log(datos$`X3 distance to the nearest MRT station`), datos$`Y house price of unit area`,
     main = "Evolución del Precio por Unidad de Área",
     xlab = "Log Distancia física hacia la estación de metro más cercana", 
     ylab = "Precio de vivienda por Unidad de Área",
     pch = 19,           
     col = "darkblue",   
     cex = 0.6)
lines(lowess(log(datos$`X3 distance to the nearest MRT station`), datos$`Y house price of unit area`), col = "red", lwd = 2)

plot(datos$`X4 number of convenience stores`, datos$`Y house price of unit area`,
     main = "Evolución del Precio por Unidad de Área",
     xlab = "Número de tiendas de conveniencia", 
     ylab = "Precio de vivienda por Unidad de Área",
     pch = 19,           
     col = "darkblue",   
     cex = 0.6)
lines(lowess(datos$`X4 number of convenience stores`, datos$`Y house price of unit area`), col = "red", lwd = 2)


plot(datos$`X5 latitude`, datos$`Y house price of unit area`,
     main = "Evolución del Precio por Unidad de Área",
     xlab = "Latitud", 
     ylab = "Precio de vivienda por Unidad de Área",
     pch = 19,           
     col = "darkblue",   
     cex = 0.6)
lines(lowess(datos$`X5 latitude`, datos$`Y house price of unit area`), col = "red", lwd = 2)

plot(datos$`X6 longitude`, datos$`Y house price of unit area`,
     main = "Evolución del Precio por Unidad de Área",
     xlab = "Longitud", 
     ylab = "Precio de vivienda por Unidad de Área",
     pch = 19,           
     col = "darkblue",   
     cex = 0.6)
lines(lowess(datos$`X6 longitude`, datos$`Y house price of unit area`), col = "red", lwd = 2)


#BOX-PLOTS#
boxplot(`Y house price of unit area` ~ factor(round(`X1 transaction date`, 2)), data = datos,
        col = "lightblue", 
        border = "darkblue",
        main = "Precio de la vivienda según la fecha de transacción",
        xlab = "Fecha de transacción", 
        ylab = "Precio por Unidad de Área",
        las = 2,           
        cex.axis = 0.7)
mediana_global <- median(datos$`Y house price of unit area`, na.rm = TRUE)
abline(h = mediana_global, col = "red", lty = 2, lwd = 2)

legend("topright", 
       legend = c("Distribución de precios por fecha de transacción", "Mediana global = 38.45"), 
       col = c("darkblue", "red"),   
       pt.bg = c("lightblue", NA),   
       pch = c(22, NA),              
       lty = c(NA, 2),               
       lwd = c(1, 2),                
       pt.cex = 1.5,                 
       cex = 0.8)

boxplot(`Y house price of unit area` ~ factor(`X4 number of convenience stores`), data = datos,
        main = "Precio de la vivienda según número de tiendas cercanas",
        xlab = "Número de tiendas de conveniencia",
        ylab = "Precio por Unidad de Área",
        col = "lightblue",
        border = "darkblue")

mediana_global <- median(datos$`Y house price of unit area`, na.rm = TRUE)
abline(h = mediana_global, col = "red", lty = 2, lwd = 2)


legend("topright", 
       legend = c("Distribución de precios por N° de tiendas", "Mediana global = 38.45"), 
       col = c("darkblue", "red"),   
       pt.bg = c("lightblue", NA),   
       pch = c(22, NA),              
       lty = c(NA, 2),               
       lwd = c(1, 2),                
       pt.cex = 1.5,                 
       cex = 0.8)

sessionInfo()


