# Aqui vctorice las edades de 7 estudiantes 
x <- c(22,55,64,76,74,23,66,37)
install.packages("tidyverse") 
install.packages("rmdformats")
install.packages("ggExtra")

library(tidyverse)
library(ggExtra)

datos = c(1,3,2,4,5,3,6,8,7) #concatenar valores
frq   = table(datos)
print(frq)

data_charcoal  = read_csv("UNdata_Charcoal.csv",show_col_types = FALSE)
charcoal_chh19 = data_charcoal %>% 
  filter(Year==2019 & Commodity=="Charcoal - Consumption by households") %>% 
  select(-Commodity)
charcoal_prd19 = data_charcoal %>% 
  filter(Year==2019 & Commodity=="Charcoal - Production") %>% 
  select(-Commodity)
#Contar cuantas unidades estadisticas (filas) hay en ell dataset
nrow(charcoal_chh19)
nrow(charcoal_prd19)

# Saber cuales son los paises que tienen ambos datasets en comun
country_ch19 = unique(charcoal_chh19$Country_Area)
country_pr19 = unique(charcoal_prd19$Country_Area)
country_comun = intersect(country_pr19,country_ch19)
length(country_comun)

# Verficar si la variable Unit es unica
country_ch19 = unique(charcoal_chh19$Unit)

#HISTOGRAMA
hist(charcoal_prd19$Quantity,
     main = 'Histograma de producción de carbón por países-áreas\nAño 2019',
     xlab = 'Intervalos de clase - Producción (en miles de toneladas métricas) de carbón',
     ylab = 'Frecuencia absoluta')


hist(charcoal_prd19$Quantity,  breaks = "Sturges",plot=FALSE,
     main = 'Histograma de producción de carbón por países-áreas\nAño 2019',
     xlab = 'Intervalos de clase - Producción (en miles de toneladas métricas) de carbón',
     ylab = 'Frecuencia absoluta')

#clase modal: es el punto medio de cada intervalo por ejemplo 250 ¿es como la moda?
#intervalo modal: es el intervalo con mayor frecuencia
#mids: son las marcas de clase
#counts: muestra los valores que se representan en las alturas de las barras
# ¿Para decir la clase modal se tiene en cuenta el intervalo modal o es el primero de $mids?

#BOXPLOT
hist(charcoal_prd19$Quantity,
     main = 'Histograma de producción de carbón por paises - áreas\nAño 2019 - con estadísticas resumen',
     xlab = 'Intervalos de clase - Producción (en miles de toneladas métricas) de carbón',
     ylab = 'Frecuencia absoluta')

abline(v = max(charcoal_prd19$Quantity),col='yellow')
abline(v = mean(charcoal_prd19$Quantity),col='blue')
abline(v = min(charcoal_prd19$Quantity),col='red')

par(mfrow=c(2,1))
hist(charcoal_prd19$Quantity,
     main = 'Histograma de producción de carbón por paises - áreas\nAño 2019 - con estadísticas resumen',
     xlab = 'Intervalos de clase - Producción (en miles de toneladas métricas) de carbón',
     ylab = 'Frecuencia absoluta',
     xlim = c(0,6500))

abline(v = max(charcoal_prd19$Quantity),col='yellow')
abline(v = mean(charcoal_prd19$Quantity),col='blue')
abline(v = min(charcoal_prd19$Quantity),col='red')

cp = boxplot(charcoal_prd19$Quantity,horizontal = TRUE,ylim = c(0,6500),
        main = 'Boxplot (orientacíon horizontal) de producción de carbón por paises - áreas\nAño 2019',
        xlab = 'Producción (en miles de toneladas métricas) de carbón')
outliers = cp$out
length(outliers)
# Ejercicio 5 
cc = boxplot(charcoal_chh19$Quantity,ylim = c(0,6500),
        main = 'Boxplot (orientacíon horizontal) de producción de carbón por paises - áreas\nAño 2019',
        xlab = 'Producción (en miles de toneladas métricas) de carbón')
outliers = cc$out
length(outliers)