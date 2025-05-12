# 0. Cargar librerías
library(tidyverse)
library(ggplot2)
library(GGally)
library(broom)
library(performance)
library(kableExtra)
library(patchwork)
library(lmtest)

# 1. cargar datos      
                      # Esto se carga en read_csv.R

student_por <- read.csv("data/student-por.csv", header = TRUE, sep = ";")

student_mat <- read.csv("data/student-mat.csv", header = TRUE, sep = ";")


d3 = merge(student_mat, student_por, by = c("school", "sex", "age", "address",
                                           "famsize", "Pstatus", "Medu", "Fedu",
                                           "Mjob", "Fjob", "reason", "nursery",
                                           "internet"))


# 2. Información general del dataset

# Para practicidad se muestran por separado los conjuntos por asignatura

student_por_num <- select(student_por, where(is.numeric))
student_mat_num <- select(student_mat, where(is.numeric))

kable(summary(student_por_num))
kable(summary(student_mat_num))



# Limpieza
# se filtran los datos de cero en matemáticas por se los outliers
student_mat_num_filtrado <- filter(student_mat_num, G1 > 0, G2 > 0, G3 > 0)


# Calcular los límites
Q1 <- quantile(student_por_num$G3, 0.25)
Q3 <- quantile(student_por_num$G3, 0.75)
IQR_value <- IQR(student_por_num$G3)

# Definir límites para detectar atípicos
limite_inferior <- Q1 - 1.5 * IQR_value
limite_superior <- Q3 + 1.5 * IQR_value

# Filtrar datos sin atípicos
student_por_num_filtrado <- student_por_num %>%
  filter(G3 >= limite_inferior & G3 <= limite_superior)




# Posible párrado
"Se hizo el filtrado de las variables numéricas que contiene el set de datos de
matemáticas, ya que así se deja por fuera aquellas variables cualitativas que no
se abordan en el presente trabajo. Del nuevo conjunto de datos obtenido se interesa
trabjar solo con las que representan las calificaciones de los y las estudiantes.
Por tal razón, se seleccionan luego las variables G1, G2 y G3 del conjunto de datos
y se grafican para entender su coheficiente correlación de Pearson, distribución
y linealidad"

# Se escogen las variables de age, studytime, G1, G2 y G3 para graficar

# Portugués
por1 <- ggplot(student_por_num_filtrado, aes(x = age)) + geom_bar(fill = "lightgreen", color = "black") + theme_minimal()
por2 <- ggplot(student_por_num_filtrado, aes(x = studytime)) + geom_bar(fill = "lightgreen", color = "black") + theme_minimal()
por3 <- ggplot(student_por_num_filtrado, aes(x = G1)) + geom_bar(fill = "lightgreen", color = "black") + theme_minimal()
por4 <- ggplot(student_por_num_filtrado, aes(x = G2)) + geom_bar(fill = "lightgreen", color = "black") + theme_minimal()
por5 <- ggplot(student_por_num_filtrado, aes(x = G3)) + geom_bar(fill = "lightgreen", color = "black") + theme_minimal()

(por1 | por2 | por3)/(por4 | por5)

# Matemáticas
mat1 <- ggplot(student_mat_num_filtrado, aes(x = age)) + geom_bar(fill = "lightblue", color = "black") + theme_minimal()
mat2 <- ggplot(student_mat_num_filtrado, aes(x = studytime)) + geom_bar(fill = "lightblue", color = "black") + theme_minimal()
mat3 <- ggplot(student_mat_num_filtrado, aes(x = G1)) + geom_bar(fill = "lightblue", color = "black") + theme_minimal()
mat4 <- ggplot(student_mat_num_filtrado, aes(x = G2)) + geom_bar(fill = "lightblue", color = "black") + theme_minimal()
mat5 <- ggplot(student_mat_num_filtrado, aes(x = G3)) + geom_bar(fill = "lightblue", color = "black") + theme_minimal()

(mat1 | mat2 | mat3)/(mat4 | mat5)


# Correlaciones

# creación de conjuntos de cada set de datos con las calificaciones para ver correlaciones entre estas.

grades_mat <- student_mat[student_mat$G1 > 0 & student_mat$G2 > 0 & student_mat$G3 > 0, c("G1", "G2", "G3")]


grades_por <- student_por[student_por$G1 > 0 & student_por$G2 > 0 & student_por$G3 > 0, c("G1", "G2", "G3")]


# Correlaciones
# notas de matemáticas:
GGally::ggpairs(grades_mat,
                lower = list(continuous = "smooth"),
                diag = list(continuous = "barDiag"),
                upper = list(continuous = wrap("cor", size = 4)))

boxplot(grades_mat)

# notas de portugués
GGally::ggpairs(grades_por,
                lower = list(continuous = "smooth"),
                diag = list(continuous = "barDiag"),
                upper = list(continuous = wrap("cor", size = 4)))
boxplot(grades_por)

"en este gráfico se pueden observar "



"Se observan variables cualitativas y cuantitativas en los datasets usados en este
ejercicio, pero solo se hará uso de las variables cuantitativas, puesto que el
modelo usado no tiene en cuenta a los primeros"

#------------------------------------------------------------------------------------

# Modelo regresión lineal
# Correlación de Pearson Matemáticas

cor(student_mat_num_filtrado$G3, student_mat_num_filtrado$G1, use = "complete.obs")
cor(student_mat_num_filtrado$G3, student_mat_num_filtrado$G2, use = "complete.obs")
cor(student_mat_num_filtrado$G3, student_mat_num_filtrado$studytime, use = "complete.obs")

# Correlación de Pearson Portugués
cor(student_por_num_filtrado$G3, student_por_num_filtrado$G1, use = "complete.obs")
cor(student_por_num_filtrado$G3, student_por_num_filtrado$G2, use = "complete.obs")
cor(student_por_num_filtrado$G3, student_por_num_filtrado$studytime, use = "complete.obs")


# Regresión matemáticas
modelo_mat <- lm(G3 ~ G2, data = student_mat_num_filtrado)
summary(modelo_mat)
#plot(modelo_mat)

modelo_por <- lm(G3 ~ G2, data = student_por_num_filtrado)
summary(modelo_por)
#plot(modelo_mat)




#------------------------------------------------------------------------------------
# diagnóstico del modelo

par(mfrow = c(2, 2))
plot(modelo_mat, col = "blue")

par(mfrow = c(2, 2))
plot(modelo_por, col = "red")


#------------------------------------------------------------------------------------

# Valores ajustados
ajuste_mat <- modelo_mat$fitted.values

ajuste_por <- modelo_por$fitted.values

# Extracción de residuales

resid_mat <- modelo_mat$residuals

resid_por <- modelo_por$residuals

# Predicciones

student_mat_num_filtrado$predicciones <- predict(modelo_mat)

student_por_num_filtrado$predicciones <- predict(modelo_por)

# dataframe con los datos de cada modelo
  #Matemáticas
resultados_mat <- data.frame(
  G2 = student_mat_num_filtrado$G2,
  G3 = student_mat_num_filtrado$G3,
  predicciones = ajuste_mat,
  residual = resid_mat
)

print(head(resultados_mat))

  #Portugués
resultados_por <- data.frame(
  G2 = student_por_num_filtrado$G2,
  G3 = student_por_num_filtrado$G3,
  predicciones = ajuste_por,
  residual = resid_por
)
print(head(resultados_por))

# recta de regresión

  # Matemáticas
ggplot(student_mat_num_filtrado, aes(x = G2, y = G3)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgreen") + #color de la regresión
  geom_segment(aes(xend = G1, yend = predicciones), col = "red", lty = "dashed") + # residuales línea roja
  geom_point() + #Puntos de datos observados
  geom_point(aes(y = predicciones), col = "red") + #puntos de predicción en rojo
  theme_light() #mejora la visualización

  # Portugués
ggplot(student_por_num_filtrado, aes(x = G2, y = G3)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightblue") + #color de la regresión
  geom_segment(aes(xend = G1, yend = predicciones), col = "red", lty = "dashed") + # residuales línea roja
  geom_point() + #Puntos de datos observados
  geom_point(aes(y = predicciones), col = "red") + #puntos de predicción en rojo
  theme_light() #mejora la visualización

# gráfico de residuos y valors ajustados
  # Matemáticas
ggplot(resultados_mat, aes(x = predicciones, y = residual)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuos vs. valores ajustados", x = "Valores ajustados", y = "Residuos") +
  theme_minimal()

  # Portugués
ggplot(resultados_por, aes(x = predicciones, y = residual)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuos vs. valores ajustados", x = "Valores ajustados", y = "Residuos") +
  theme_minimal()


# Histograma de residuos
  #Matemáticas
qqnorm(resid_mat)
qqline(resid_mat, col = "red")

  # Portugués
qqnorm(resid_por)
qqline(resid_por, col = "blue")

#test de normalidad
  # Matemáticas
shapiro.test(resid_mat)

  # Portugués
shapiro.test(resid_por)


#test de homocedasticidad

# Matemáticas
bptest(modelo_mat)

  # Portugués
bptest(modelo_por)


