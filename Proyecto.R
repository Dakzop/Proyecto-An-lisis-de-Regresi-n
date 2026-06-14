library(readxl)
library(ggplot2)
library(dplyr)
library(GGally)
library(moments)
library(leaps)
library(lmtest)
library(tseries)
library(nortest)
library(MASS)
library(car)
library(VisCollin)
library(lubridate)

datos <- read_xlsx("C:/Users/nelso/Desktop/Regresion/Real estate valuation data set.xlsx") #Colocar aquí la ruta del archivo#

########################################################################################################
#EDA

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



#PLOT DE LAS VARIABLES CON LAS VARIABLES RESPUESTA#
par(mar = c(8, 5, 4, 2))

meses_nombres <- c("Jun 2012.", "Jul 2012.", "Ago 2012.", "Sep 2012.", "Oct 2012.", 
                   "Nov 2012.", "Dic 2012.", "Ene 2013.", "Feb 2013.", "Mar 2013.", 
                   "Abr 2013.", "May 2013.")

plot(datos$`X1 transaction date`, datos$`Y house price of unit area`,
     main = "Evolución del Precio por Unidad de Área",
     xlab = "",              
     ylab = "Precio de vivienda por Unidad de Área", 
     cex.lab = 1.5,
     cex.main = 1.7,
     pch = 19, col = "darkblue", cex = 0.7, xaxt = "n")

posiciones_fechas <- seq(min(datos$`X1 transaction date`), max(datos$`X1 transaction date`), length.out = 12)

axis(1, at = posiciones_fechas, labels = meses_nombres, las = 2, cex.axis = 1)

mtext("Mes de Transacción", side = 1, line = 6, cex = 1.5)
lines(lowess(datos$`X1 transaction date`, datos$`Y house price of unit area`), col = "red", lwd = 2)

par(xpd = TRUE)
legend("topleft", 
       legend = c("Observaciones", "Tendencia LOWESS"),
       col = c("darkblue", "red"),
       pch = c(19, NA), lty = c(NA, 1), lwd = c(NA, 2), cex = 1.5)
par(xpd = FALSE)

par(mar = c(8, 5, 4, 2))

plot(datos$`X2 house age`, datos$`Y house price of unit area`,
     main = "Evolución del Precio por Unidad de Área",
     xlab = "",
     ylab = "Precio de vivienda por Unidad de Área", 
     cex.lab = 1.5,
     cex.main = 1.7,
     pch = 19, col = "darkblue", cex = 0.7)

mtext("Antigüedad de la vivienda", side = 1, line = 6, cex = 1.5)

lines(lowess(datos$`X2 house age`, datos$`Y house price of unit area`), col = "red", lwd = 2)

legend("topright", 
       legend = c("Observaciones", "Tendencia LOWESS"),
       col = c("darkblue", "red"),
       pch = c(19, NA), lty = c(NA, 1), lwd = c(NA, 2), cex = 1.5)

hist(
  datos$`X3 distance to the nearest MRT station`,
  freq = TRUE,                
  col = rgb(0.2, 0.6, 0.8, 0.7),
  border = "#1a5276",          
  main = "Distribución de la estación de metro más cercana",
  xlab = "Distancia física hacia la estación de metro más próxima",
  ylab = "Frecuencia",
  cex.lab = 1.5,
  font.main = 2,                
  cex.main = 1.7,
  las = 1,                      
)

z_scores_X3 <- scale(datos$`X3 distance to the nearest MRT station`)
valores_atipicos <- which(abs(z_scores_X3) > 3)
print(valores_atipicos)
distancias_atipicas <- datos$`X3 distance to the nearest MRT station`[valores_atipicos]
print(distancias_atipicas)
rug(distancias_atipicas, col = "red", lwd = 2, ticksize = 0.15)

legend("topright", 
       legend = c("Distribución de viviendas", "Valores atípicos (|Z| > 3)"),
       col = c("#1a5276", "red"),                 
       pt.bg = c(rgb(0.2, 0.6, 0.8, 0.7), NA),    
       pch = c(22, NA),                          
       lty = c(NA, 1),                            
       lwd = c(1, 2),                            
       pt.cex = 2,                               
       bty = "n",                                
       cex = 1.5)                                

par(mar = c(5.5, 5.5, 4, 2))

plot(datos$`X3 distance to the nearest MRT station`, datos$`Y house price of unit area`,
     main = "Evolución del Precio por Unidad de Área",
     xlab = "Distancia física hacia la estación de metro más cercana", 
     ylab = "Precio de vivienda por Unidad de Área",
     cex.lab = 1.5,
     cex.main = 1.7,
     pch = 19, col = "darkblue", cex = 0.7)

lines(lowess(datos$`X3 distance to the nearest MRT station`, datos$`Y house price of unit area`), col = "red", lwd = 2)

legend("topright", 
       legend = c("Observaciones", "Tendencia LOWESS"),
       col = c("darkblue", "red"),
       pch = c(19, NA),       
       lty = c(NA, 1),        
       lwd = c(NA, 2),        
       cex = 1.5)

plot(log(datos$`X3 distance to the nearest MRT station`), datos$`Y house price of unit area`,
     main = "Evolución del Precio por Unidad de Área",
     xlab = "Log Distancia física hacia la estación de metro más cercana", 
     ylab = "Precio de vivienda por Unidad de Área",
     cex.lab = 1.5,
     cex.main = 1.7,
     pch = 19, col = "darkblue", cex = 0.7)

lines(lowess(log(datos$`X3 distance to the nearest MRT station`), datos$`Y house price of unit area`), col = "red", lwd = 2)

legend("topright", 
       legend = c("Observaciones", "Tendencia LOWESS"),
       col = c("darkblue", "red"),
       pch = c(19, NA),       
       lty = c(NA, 1),        
       lwd = c(NA, 2),        
       cex = 1.5)

#HISTOGRAMAS

hist(
  datos$`Y house price of unit area`,
  freq = TRUE,                
  col = rgb(0.2, 0.6, 0.8, 0.7),
  border = "#1a5276",          
  main = "Distribución de los precios por únidad de área",
  xlab = "Precio de la vivienda por unidad de área",
  ylab = "Frecuencia",
  cex.lab = 1.5,
  font.main = 2,                
  cex.main = 1.7,               
  las = 1,                      
)
z_scores_Y <- scale(datos$`Y house price of unit area`)
valores_atipicos_Y <- which(abs(z_scores_Y) > 3)
precios_atipicos <- datos$`Y house price of unit area`[valores_atipicos_Y]
rug(precios_atipicos, col = "red", lwd = 2, ticksize = 0.05)
print(valores_atipicos_Y)
print(precios_atipicos)

legend("topright", 
       legend = c("Distribución de viviendas", "Valores atípicos (|Z| > 3)"),
       col = c("#1a5276", "red"),                 
       pt.bg = c(rgb(0.2, 0.6, 0.8, 0.7), NA),    
       pch = c(22, NA),                           
       lty = c(NA, 1),                            
       lwd = c(1, 2),                             
       pt.cex = 2,                                
       bty = "n",                                 
       cex = 1.5)

plot(datos$`X4 number of convenience stores`, datos$`Y house price of unit area`,
     main = "Evolución del Precio por Unidad de Área",
     xlab = "Número de tiendas de conveniencia", 
     ylab = "Precio de vivienda por Unidad de Área",
     cex.lab = 1.5,
     cex.main = 1.7,
     pch = 19, col = "darkblue", cex = 0.7)

lines(lowess(datos$`X4 number of convenience stores`, datos$`Y house price of unit area`), col = "red", lwd = 2)

legend("topright", 
       legend = c("Observaciones", "Tendencia LOWESS"),
       col = c("darkblue", "red"),
       pch = c(19, NA),       
       lty = c(NA, 1),        
       lwd = c(NA, 2),        
       cex = 1.5)

plot(datos$`X5 latitude`, datos$`Y house price of unit area`,
     main = "Evolución del Precio por Unidad de Área",
     xlab = "Latitud", 
     ylab = "Precio de vivienda por Unidad de Área",
     cex.lab = 1.5,
     cex.main = 1.7,
     pch = 19, col = "darkblue", cex = 0.7)

lines(lowess(datos$`X5 latitude`, datos$`Y house price of unit area`), col = "red", lwd = 2)


legend("topright", 
       legend = c("Observaciones", "Tendencia LOWESS"),
       col = c("darkblue", "red"),
       pch = c(19, NA),       
       lty = c(NA, 1),        
       lwd = c(NA, 2),        
       cex = 1.5)

plot(datos$`X6 longitude`, datos$`Y house price of unit area`,
     main = "Evolución del Precio por Unidad de Área",
     xlab = "Longitud", 
     ylab = "Precio de vivienda por Unidad de Área",
     cex.lab = 1.5,
     cex.main = 1.7,
     pch = 19, col = "darkblue", cex = 0.7)

lines(lowess(datos$`X6 longitude`, datos$`Y house price of unit area`), col = "red", lwd = 2)

legend("topright", 
       legend = c("Observaciones", "Tendencia LOWESS"),
       col = c("darkblue", "red"),
       pch = c(19, NA),       
       lty = c(NA, 1),        
       lwd = c(NA, 2),        
       cex = 1.5)

#BOX-PLOT
valores_unicos <- sort(unique(datos$`X1 transaction date`))

meses_nombres <- c("Jun 2012.", "Jul 2012.", "Ago 2012.", "Sep 2012.", "Oct 2012.", 
                   "Nov 2012.", "Dic 2012.", "Ene 2013.", "Feb 2013.", "Mar 2013.", 
                   "Abr 2013.", "May 2013.")

par(mar = c(8, 5, 5, 2))

boxplot(`Y house price of unit area` ~ factor(`X1 transaction date`, levels = valores_unicos), 
        data = datos,
        col = "lightblue", 
        border = "darkblue",
        main = "Precio de la vivienda según mes de transacción",
        xlab = "", 
        ylab = "Precio por Unidad de Área",
        las = 2,            
        xaxt = "n",
        cex.axis = 0.8,
        cex.lab = 1.5,
        cex.main = 1.7)

axis(1, at = 1:length(valores_unicos), labels = meses_nombres, las = 2, cex.axis = 0.8)

mtext("Mes de Transacción", side = 1, line = 6, cex = 1.5)
abline(h = median(datos$`Y house price of unit area`, na.rm = TRUE), col = "red", lty = 2, lwd = 2)

legend("topleft", 
       legend = c("Distribución de precios", "Mediana global"), 
       col = c("darkblue", "red"),   
       pt.bg = c("lightblue", NA),   
       pch = c(22, NA),              
       lty = c(NA, 2), 
       lwd = c(1, 2),                
       pt.cex = 1.5,                 
       cex = 1.4,
       bty = "n")

par(mar = c(8, 5, 5, 2))

boxplot(`Y house price of unit area` ~ factor(`X4 number of convenience stores`), 
        data = datos,
        main = "",              
        xlab = "",              
        ylab = "Precio por Unidad de Área",
        col = "lightblue",
        border = "darkblue",
        cex.axis = 0.8,
        cex.lab = 1.5)

mtext("Precio de la vivienda según número de tiendas cercanas", side = 3, line = 2.5, cex = 1.7, font = 2)

mtext("Número de tiendas de conveniencia", side = 1, line = 4, cex = 1.5)

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
       cex = 1.5,
       bty = "n")

# Test rápido para el efecto temporal (Fecha vs Precio)
cor.test(datos$`Y house price of unit area`,datos$`X1 transaction date`)

# Test rápido para número de tiendas (Tiendas vs Precio)
cor.test(datos$`Y house price of unit area`,datos$`X4 number of convenience stores`)


###########################################################################################
##FIN DEL EDA##
###########################################################################################
#MODELOS

modelo_completo <- lm(`Y house price of unit area` ~ 
                        `X1 transaction date` + 
                        `X2 house age` + 
                        `X3 distance to the nearest MRT station` + 
                        `X4 number of convenience stores` +
                        `X5 latitude` + 
                        `X6 longitude`, 
                      data = datos)
summary(modelo_completo)

#TRANSFORMACIÓN BOX-COX#
resultado_boxcox <- boxcox(modelo_completo, lambda = seq(-2, 2, by = 0.1), plotit = FALSE)
df_bc <- data.frame(lambda = resultado_boxcox$x, loglik = resultado_boxcox$y)
lambda_optimo <- resultado_boxcox$x[which.max(resultado_boxcox$y)]

grafico_boxcox <- ggplot(df_bc, aes(x = lambda, y = loglik)) +
  geom_line(color = "#1a5276", size = 1.2) +                  
  geom_vline(aes(xintercept = lambda_optimo, color = "Lambda Óptimo"), 
             linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = 0, color = "Lambda = 0 (Transformación Log)"), 
             linetype = "dotted", size = 1) +
  
  annotate("text", x = lambda_optimo + 0.4, y = min(df_bc$loglik) + 10, 
           label = paste("Óptimo:", round(lambda_optimo, 3)), 
           color = "red", fontface = "bold", size = 5) +

  scale_color_manual(name = "Líneas de referencia:", 
                     values = c("Lambda Óptimo" = "red", 
                                "Lambda = 0 (Transformación Log)" = "gray50")) +
  
  labs(x = expression(Valores~de~lambda),
       y = "Log-Verosimilitud",
       title = "") +                                          
  theme_classic() +                                           
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    legend.position = "top",                                
    legend.title = element_text(face = "bold", size = 16),    
    legend.text = element_text(size = 16),
    plot.margin = margin(16, 16, 16, 16)
  )

print(grafico_boxcox)

#INTERVALO DE CONFIANZA MODELO ORIGINAL COMPLETO#
intervalos_confi <- confint(modelo_completo, level = 0.95)

#################################################################################

#NUEVA VARIABLE X_DEPI#
indice_epicentro <- which.max(datos$`Y house price of unit area`)

lat_epicentro <- datos$`X5 latitude`[indice_epicentro]
lon_epicentro <- datos$`X6 longitude`[indice_epicentro]

datos$X_depi <- sqrt((datos$`X5 latitude` - lat_epicentro)^2 + 
                       (datos$`X6 longitude` - lon_epicentro)^2)

datos$lat_centrada <- datos$`X5 latitude` - mean(datos$`X5 latitude`)
datos$lon_centrada <- datos$`X6 longitude` - mean(datos$`X6 longitude`)

#MODELOS

modelo1 <- lm(log(`Y house price of unit area`) ~ 
                `X1 transaction date` + sqrt(`X2 house age`) + 
                `X4 number of convenience stores` +
                X_depi, data = datos)

modelo2 <- lm(log(`Y house price of unit area`) ~ 
                `X1 transaction date` + 
                sqrt(`X2 house age`) + 
                log(`X3 distance to the nearest MRT station`) + 
                `X4 number of convenience stores` +
                X_depi, data = datos)


summary(modelo1)
summary(modelo2)

#TABLA ANOVA DE MODELOS#
anova(modelo1, modelo2)

#CRITERIOS AIC Y BIC#
AIC(modelo1, modelo2)
BIC(modelo1, modelo2)

#INTERVALO DE CONFIANZA#
confint(modelo2, level = 0.95)

####################################################################
#FIN DE LA ELECCIÓN DE MODELO#
#####################################################################

#INCIO DEL DIGANOSTICO#

#TEST DE RAMSEY#
resettest(modelo2)

#Media de los residuos#
mean(residuals(modelo2))

#QQPLOT#
par(mar = c(5, 5, 4, 3))
qqPlot(
  modelo2,
  id = list(n = 3),      
  pch = 19,              
  col = "darkblue",
  col.lines = "red",
  envelope = list(col = "darkblue"),
  grid = FALSE,
  main = "", 
  xlab = "", 
  ylab = "",
  cex = 0.7,
  cex.id = 0.3,
  cex.axis = 1.5
)

mtext("QQ-Plot de los Residuos", side = 3, line = 1, cex = 1.7) 
mtext("Cuantiles Teóricos", side = 1, line = 3, cex = 1.5)     
mtext("Residuos Studentizados", side = 2, line = 3, cex = 1.5) 

#TEST DE BREUSCH-PAGAN#
bptest(modelo2)

#GRAFICO HETEROCEDASTICIDAD#
res <- residuals(modelo2)
plot(fitted(modelo2), residuals(modelo2),
     main = "Residuos vs Valores Ajustados",
     cex.main = 1.7,     
     cex.lab = 1.5,
     cex.axis = 1.1,
     pch = 1,
     col = "darkblue",
     cex = 0.7,
     xlab = "Valores ajustados",
     ylab = "Residuos")

abline(h = 0, lty = 2)

idx <- which(abs(rstudent(modelo2)) > 3)

text(fitted(modelo2)[idx],
     res[idx],
     labels = idx,
     cex = 0.7,      
     pos = 4)

#TEST DE NORMALIDAD DE RESIDUOS (SHAPIRO-WILK Y JARQUE-BERA)#
shapiro.test(residuals(modelo2))
jarque.bera.test(residuals(modelo2))

#CALCULO DE VIF Y SU GRÁFICO#
vif(modelo2)

barplot(vif(modelo2), names.arg = c("F. Trans", "E. Casa", "Log(D. MRT)", "N. Tiendas", "D. Epicentro" ) , 
        main = "Valores VIF por Variable", 
        col = "darkblue", las = 1, cex.names = 1,  ylim = c(0, 12))
abline(h = 10, col = "red", lty = 2, lwd = 2)
legend("topright", 
       legend = "Umbral VIF > 10",
       col = "red",
       pch = NA,       
       lty = 1,        
       lwd = 2,        
       cex = 1)

#ÍNDICES DE CONDICIÓN#
colldiag(modelo2)

#REVISION DE VALORES LLAMATIVOS#
diagnostico <- data.frame(
  Fila = 1:nrow(datos),
  Residuo_Est = rstandard(modelo2),
  Leverage = hatvalues(modelo2),
  Cook = cooks.distance(modelo2) 
)

p <- length(coef(modelo2))
n <- nrow(datos)
umbral_leverage <- 3 * p / n

casos_raros <- subset(diagnostico, abs(Residuo_Est) > 3 | Leverage > umbral_leverage | Cook > 1)
head(casos_raros,10)


#TEST de Outliers

casos <- c(114, 149, 271)


dfb <- dfbetas(modelo2)[casos] 
dff <- dffits(modelo2)[casos]    
cov <- covratio(modelo2)[casos]  

# Crear tabla resumen
tabla_influencia <- data.frame(
  Fila = casos,
  DFBETAS = round(dfb,4),
  DFFITS = round(dff, 4),
  COVRATIO = round(cov, 4)
)
print(tabla_influencia)

#ANÁLISIS DE SENSIBILIDAD SIN DATOS SELECCIONADOS#

datos_limpios <- datos[-c(114, 149, 271), ]

modelo2_limpio <- lm(log(`Y house price of unit area`) ~ 
                `X1 transaction date` + 
                sqrt(`X2 house age`) + 
                log(`X3 distance to the nearest MRT station`) + 
                `X4 number of convenience stores` +
                X_depi, data = datos_limpios)

summary(modelo2)
summary(modelo2_limpio)
confint(modelo2, level = 0.95)
confint(modelo2_limpio, level = 0.95)


#R CONFIG#
sessionInfo()
