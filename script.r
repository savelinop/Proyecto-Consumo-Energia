# 1. Cargar las librerías necesarias
library(ggplot2) # Para gráficos
library(dplyr)    # Para manipulación de datos
library(stats)    # Para regresión y análisis estadísticos
library(reshape2)



# 2. Cargar el dataset
df <- read.csv('base_consumo.csv', encoding = "ISO-8859-1")

# 3. Verificar las primeras filas de los datos
head(df)

# 4. Limpiar los datos 
# Revisar valores faltantes
sum(is.na(df))

# Eliminar filas con NA 
df <- na.omit(df)

# Estadistica Descriptiva de Variables Numericas

# 5. Análisis descriptivo de las variables
summary(df)

# 6. Gráfico de dispersión entre el número de personas y el consumo de energía
ggplot(df, aes(x = numero.habitantes, y = consumo.kwh.mensual)) +
  geom_point() +
  labs(title = "Diagrama de dispersión entre Número de Personas y Consumo de Energía",
       x = "Número de Personas en el Hogar",
       y = "Consumo de Energía (kWh mensual)") +
  theme_minimal()

# 7. Ajustar el modelo de regresión lineal simple
modelo <- lm(consumo.kwh.mensual ~ numero.habitantes, data = df)

# 8. Resumen del modelo (coeficientes, R^2, p-valor)
summary(modelo)

# 9. Evaluar el modelo con el coeficiente de determinación (R^2) y p-valor
# El valor de R^2 indica qué porcentaje de la variabilidad del consumo de energía se explica por el modelo
# El p-valor asociado al coeficiente de nro_habitantes indica si es significativo


# 10. Realizar las predicciones del consumo de energía
predicciones <- predict(modelo)

# 11. Graficar los resultados del modelo (línea de regresión sobre el diagrama de dispersión)
ggplot(df, aes(x = numero.habitantes, y = consumo.kwh.mensual)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Relación entre Número de Personas y Consumo de Energía (Regresión Lineal)",
       x = "Número de Personas en el Hogar",
       y = "Consumo de Energía (kWh mensual)") +
  theme_minimal()

# 12. Evaluar el modelo: 
# Coeficientes de la regresión
coef(modelo)

# Error estándar de las estimaciones
summary(modelo)$coefficients

# 13. Evaluar la significancia del modelo (p-valor y R^2)
p_valor <- summary(modelo)$coefficients[2,4]  # p-valor del coeficiente de nro_habitantes
r2 <- summary(modelo)$r.squared  # Coeficiente de determinación

cat("El p-valor es:", p_valor, "\n")
cat("El valor de R^2 es:", r2, "\n")

# 14. Conclusiones
# Si el p-valor es bajo (por ejemplo, < 0.05) y el R^2 es alto, se puede concluir que existe una relación significativa entre el número de personas y el consumo de energía.

# 15. Gráfico de residuos
residuos <- residuals(modelo)

ggplot(data.frame(residuos), aes(x = residuos)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribución de los Residuos del Modelo de Regresión",
       x = "Residuos", y = "Frecuencia") +
  theme_minimal()

# 16. Gráfico de Predicciones vs Valores Reales
ggplot(df, aes(x = consumo.kwh.mensual, y = predicciones)) +
  geom_point(color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Comparación entre los Valores Reales y Predichos",
       x = "Valores Reales de Consumo de Energía",
       y = "Valores Predichos de Consumo de Energía") +
  theme_minimal()

# 18. Gráfico de la línea de regresión con intervalos de confianza
ggplot(df, aes(x = numero.habitantes, y = consumo.kwh.mensual)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Línea de Regresión con Intervalos de Confianza",
       x = "Número de Personas en el Hogar",
       y = "Consumo de Energía (kWh mensual)") +
  theme_minimal()


# 20. Boxplot para la variable dependiente
ggplot(df, aes(y = consumo.kwh.mensual)) +
  geom_boxplot(fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Boxplot del Consumo de Energía Mensual",
       y = "Consumo de Energía (kWh mensual)") +
  theme_minimal()

# 21. Estadística descriptiva adicional (IQR, sesgo, curtosis)


# 21.1. Rango intercuartílico (IQR)
IQR(df$consumo.kwh.mensual)

# 21.2. Sesgo (cálculo manual sin moments)
media <- mean(df$consumo.kwh.mensual)
desv <- sd(df$consumo.kwh.mensual)
n <- length(df$consumo.kwh.mensual)

sesgo <- sum((df$consumo.kwh.mensual - media)^3) / (n * desv^3)
sesgo   # Mostrar el sesgo

# 21.3. Curtosis (cálculo manual sin moments)
curtosis <- sum((df$consumo.kwh.mensual - media)^4) / (n * desv^4)
curtosis   # Mostrar la curtosis

# 22. Variables categóricas (frecuencias y proporciones)

# 22.1. Frecuencias de tipo de vivienda
table(df$Material_Vivienda)

# 22.2. Proporciones de tipo de vivienda
prop.table(table(df$Material_Vivienda))

# 22.3. Gráfico de barras para variable categórica
ggplot(df, aes(x = Material_Vivienda)) +
  geom_bar(fill = "skyblue", color = "black", alpha = 0.9) +
  labs(title = "Frecuencia por Tipo de Vivienda",
       x = "Tipo de Vivienda",
       y = "Conteo") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13)
  )


# 23. Matriz de correlación sin corrplot
num_vars <- df[sapply(df, is.numeric)]
corr_matrix <- cor(num_vars)

# Mostrar la matriz en consola
corr_matrix

# Gráfico de calor de correlación sin corrplot
ggplot(melt(corr_matrix), aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "blue",
    high = "red",
    mid = "white",
    midpoint = 0,
    limit = c(-1,1)
  ) +
  theme_minimal() +
  labs(
    title = "Matriz de Correlación",
    x = "", y = ""
  ) +
  theme(
    axis.text.x = element_text(
      angle = 45,      # Rotar texto
      hjust = 1,       # Ajustar posición a la derecha
      size = 12,       # Tamaño más grande
      face = "bold"    # Negrita para mejor lectura
    ),
    axis.text.y = element_text(
      size = 12,
      face = "bold"
    ),
    plot.title = element_text(size = 16, face = "bold")
  )

# 24. Tabla de contingencia y prueba Chi-cuadrado

# 24.1. Tabla región vs Material de vivienda
tabla_cont <- table(df$Region, df$Material_Vivienda)
tabla_cont

# 24.2. Prueba chi cuadrado
chisq.test(tabla_cont)

# 25. Prueba t de diferencia de medias

# Crear una variable categórica según el tamaño del área construida
df$Grupo_Area <- ifelse(df$area.construccion.metros.cuadrados <
                          median(df$area.construccion.metros.cuadrados),
                        "Pequeña", "Grande")

# Ver niveles (opcional)
table(df$Grupo_Area)

# Comparar consumo entre casas pequeñas y grandes
t.test(consumo.kwh.mensual ~ Grupo_Area, data = df)

# 26. Segundo modelo de regresión lineal 

# 26.1. Modelo 2: área de construcción → consumo
modelo2 <- lm(area.construccion.metros.cuadrados ~ consumo.kwh.mensual, data = df)

# 26.2. Resumen del modelo 2
summary(modelo2)

# 26.3. Gráfico del modelo 2
ggplot(df, aes(x = area.construccion.metros.cuadrados, y = consumo.kwh.mensual)) +
  geom_point(color = "green") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Modelo 2: Área de Construcción vs Consumo",
       x = "Área Construida (m2)",
       y = "Consumo (kWh mensual)") +
  theme_minimal()

# 27. Comparación entre los dos modelos de regresión

# 27.1. R² de ambos modelos
summary(modelo)$r.squared
summary(modelo2)$r.squared

# 27.2. AIC de ambos modelos
AIC(modelo)
AIC(modelo2)




