# Proyecto: Relación entre Factores del Hogar y el Consumo Energético

Este proyecto tiene como objetivo analizar cómo distintos factores del hogar, como el número de habitantes, el área de construcción, el material de la vivienda, el aislamiento térmico y el uso de aire acondicionado, influyen en el consumo energético mensual. Para ello, se aplican técnicas de estadística descriptiva, análisis bivariante, pruebas inferenciales y modelos de regresión lineal simple. A través de este análisis buscamos comprender qué variables del hogar explican mejor el consumo de energía y cómo estas relaciones pueden utilizarse para realizar predicciones confiables.

---

## Objetivos

1. **Objetivo 1:** Describir el comportamiento de las variables del hogar mediante estadísticas descriptivas y visualizaciones adecuadas.

2. **Objetivo 2:** Determinar la relación entre diferentes factores del hogar (habitantes, materiales, área, AC, región y el consumo energético mensual.

3. **Objetivo 3:** Evaluar la significancia estadística de estas relaciones mediante análisis bivariado, correlación, tablas de contingencia y pruebas inferenciales.

4. **Objetivo 4:** Construir y comparar al menos dos modelos de regresión lineal simple utilizando distintas variables predictoras del dataset.

5. **Objetivo 5:** Identificar cuál modelo de regresión ofrece el mejor desempeño según criterios como el coeficiente de determinación (R²), el error estándar y la significancia de los coeficientes.

---

### Técnicas estadísticas utilizadas:

- **Estadística Descriptiva:** Cálculo de media, mediana, desviación estándar, rango intercuartílico, sesgo y curtosis para analizar el comportamiento de las variables numéricas consumo kWh, área, número de habitantes, gasto.

- **Análisis de Frecuencias:** Obtención de frecuencias absolutas y relativas para variables categóricas como región, zona, material de vivienda, aislamiento térmico, uso de calentador y presencia de aire acondicionado.

- **Análisis Bivariante:**  
  - Matriz de correlación para evaluar la relación entre variables numéricas.  
  - Tablas de contingencia para analizar variables categóricas.  
  - Prueba **Chi-cuadrado** para evaluar independencia entre variables categóricas.

- **Prueba t para diferencia de medias:** Comparación del consumo energético entre grupos definidos por características del hogar (por ejemplo: con AC vs. sin AC).

- **Regresión Lineal Simple:** Construcción de al menos dos modelos:  
  - Modelo 1: *Número de habitantes → Consumo energético*.  
  - Modelo 2: *Área de construcción → Consumo energético*.  

- **Evaluación del Modelo:**  
  Uso del coeficiente de determinación **(R²)**, error estándar, p-valores y significancia de los coeficientes para comparar modelos y seleccionar el más adecuado.

---

### Gráficos Generados:

- **Histogramas:** Distribución del consumo energético y variables numéricas relevantes.
- **Diagramas de caja (boxplots):** Identificación de outliers por región, zona y material de vivienda.
- **Gráficos de barras:** Visualización de frecuencias para materiales, aislamiento, zona y AC.
- **Diagrama de dispersión:** Relación entre habitantes y consumo energético, y entre área de construcción y consumo.
- **Línea de regresión:** Inserción de la recta ajustada en los diagramas de dispersión.
- **Mapa de calor de correlación:** Visualización de relaciones lineales entre variables numéricas.
- **Gráfico de residuos:** Evaluación de supuestos del modelo de regresresión.
- **Distribución de residuos:** Verificación de normalidad y patrones en los errores.
- **Predicciones vs. valores reales:** Comparación del desempeño de los modelos construidos.

---

## Conclusiones

- Existe una **relación significativa** entre varios factores del hogar (como número de habitantes, área de construcción y uso de aire acondicionado) y el consumo energético mensual. Estos factores muestran asociaciones claras en los análisis descriptivos y bivariantes.

- Los modelos de **regresión lineal simple** construidos indicaron que tanto el número de habitantes como el área de construcción explican parte importante de la variabilidad del consumo, aunque ningún modelo por sí solo captura toda la complejidad del comportamiento energético.

- El segundo modelo (área de construcción → consumo energético) mostró un **mejor desempeño** en términos de R², error estándar y significancia de los coeficientes, convirtiéndose en el modelo más adecuado según los criterios del proyecto.

- El análisis de gráficos, residuos y correlaciones sugiere que el consumo energético está influenciado por múltiples factores del hogar, por lo que se recomienda considerar modelos más avanzados (múltiple o no lineal) en estudios futuros.

- Se evidencia que la estadística permite identificar patrones reales en los hogares y proporciona herramientas sólidas para interpretar y predecir el consumo energético residencial.

---

## Archivos del Proyecto

- **base_consumo.csv** : Conjunto de datos utilizado en todos los análisis.  
- **script.R** : Código en R con el análisis descriptivo, bivariado, inferencial y modelos de regresión.  
- **consumo_energia_hogar.Rproj** : Proyecto de RStudio para ejecutar y organizar los scripts.  

---

## Instrucciones para ejecutar el código
- **Clonar Repositorio** : https://github.com/savelinop/Proyecto-Consumo-Energia.git
- Ejecutar en R Studio: En caso de tener problemas con el archivo base_consumo, usar el código en script.R
