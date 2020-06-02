library(readxl)
library(tidyverse)

setwd("C:/Users/henry/OneDrive - Universidad Nacional Autónoma de Honduras/Investigaciones Henry")

df <- read_excel("Covid19/Casos de Covid19.xlsx", sheet = 'Pruebas')

# Regresiones ----

mean(df$`% Positivas`)

modelo <- lm(Positivas ~ Pruebas, data = df)
summary(modelo)

par(mfrow=c(2,2))
plot(modelo)

modelo.pot <- lm(log(Positivas) ~ log(Pruebas), data = df[-1,])
summary(modelo.pot)
plot(modelo.pot)
exp(modelo.pot$coefficients)

# Gráficos  ----

par(mfrow = c(1,3))

plot(df$Pruebas, df$Positivas, xlab = 'Pruebas', ylab = 'Pruebas +', main = 'Pruebas vs. Pruebas +') +
  lines(df$Pruebas, exp(predict(modelo.pot, df)))

plot(df$`% Positivas`*100, ylab = '%', xlab = 'Time', main = '% Pruebas +') +
  lines(df$`% Positivas`*100) +
  abline(h = 22)

df$Variación <- (df$`% Positivas` - lag(df$`% Positivas`))/lag(df$`% Positivas`)

plot(df$Variación*100, ylab = '▲ %', xlab = 'Time', main = 'Variación % Pruebas +') +
  lines(df$Variación*100) +
  abline(h = 0)
