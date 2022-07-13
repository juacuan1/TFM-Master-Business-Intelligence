#Índice

# I.-       Instalación de paquetes y librerías.
# II.-      Lectura de fichero.
# III.-     Copia de los datos para trabajar.
# IV.-      Documentación y selección de variables.
# IV.a.-    Renombrar variables para facilitar la comprensión.
# V.-       Análisis inicial de los datos.
# VI.-      Limpieza de los datos.
# VII.-     Transformación de los datos.
# VIII.-    Análisis exploratorio de los datos.
# VIII.a.-  Matriz de correlación para visualizar relaciones con la variable dependiente.
# IX.-      Diseño y desarrollo de modelos de aprendizaje supervisado.
# IX.a.-    Partición de los datos en los conjuntos de prueba y entrenamiento
# IX.b.-    Creación de datasets balanceados mediante la técnica de Undersampling.
# IX.c.-    Implementación de modelos
# X.-       Métricas y resultados 





# I.- Instalación de paquetes y librerías.

install.packages("foreign")
install.packages("reshape")
install.packages("gmodels")
install.packages("Hmisc")
install.packages("GGally")
install.packages("corrplot")
install.packages("PerformanceAnalytics")
install.packages("tidyverse")
install.packages("corrplot")
install.packages("caTools")
install.packages("caret")
install.packages("ROSE")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("vcd")
install.packages("MASS")
install.packages("randomForest")
install.packages("ROCR")
install.packages("ranger")
install.packages("MLmetrics")
install.packages("naivebayes")
install.packages("neuralnet")
install.packages("e10712")

library(foreign)
library(reshape)
library(gmodels)
library(Hmisc)
library(GGally)
library(corrplot)
library(PerformanceAnalytics)
library(tidyverse) 
library(corrplot)
library(caTools)
library(caret)
library(ROSE)
library(rpart)
library(rpart.plot)
library(vcd)
library(MASS)
library(randomForest)
library(ROCR)
library(ranger)
library(MLmetrics)
library(naivebayes)
library(neuralnet)
library(e1071)





# II.- Lectura de fichero.

file2015 <- read.xport("C:/Users/Usuario/Desktop/Máster BI/TFM/Datasets/LLCP2015.XPT")





# III.- Copia de los datos para trabajar.

datos <- file2015





# IV.- Documentación y selección de variables tras consultar la documentación y  
#       estudiar las variables más significativas para este caso de uso.

datos$DIABETE3    # 0.-  Diabetes (Var. dependiente).
datos$X_RFHYPE5   # 1.-  Presión arterial.
datos$TOLDHI2     # 2.-  Colesterol alto. 
datos$X_CHOLCHK   # 3.-  Revisión del colesterol en los últimos 5 años.
datos$X_BMI5      # 4.-  Índice de masa muscular (IMC).
datos$SMOKE100    # 5.-  Fumador.
datos$CVDSTRK3    # 6.-  Ictus.
datos$X_MICHD     # 7.-  Enfermedad coronaria del corazón o infarto de miocardio.
datos$X_TOTINDA   # 8.-  Actividad física.
datos$X_FRTLT1    # 9.-  Consumo de fruta.
datos$X_VEGLT1    # 10.- Consumo de verdura.
datos$X_RFDRHV5   # 11.- Consumo excesivo de alcohol.
datos$HLTHPLN1    # 12.- Acceso a atención sanitaria.
datos$MEDCOST     # 13.- No ha podido visitar un médico en los últimos 12 meses debido a los costes.
datos$GENHLTH     # 14.- Estado general de salud.
datos$MENTHLTH    # 15.- Número de días del mes que ha tenido una mala salud mental.
datos$PHYSHLTH    # 16.- Número de días del mes que ha tenido una mala salud física.
datos$DIFFWALK    # 17.- Dificultad para caminar o subir escaleras.
datos$SEX         # 18.- Sexo.
datos$X_AGEG5YR   # 19.- Edad.
datos$EDUCA       # 20.- Educacion.
datos$INCOME2     # 21.- Ingresos.

datos <- subset(datos, TRUE, select = c(DIABETE3, X_RFHYPE5, TOLDHI2, X_CHOLCHK, X_BMI5, SMOKE100,
                                        CVDSTRK3, X_MICHD, X_TOTINDA, X_FRTLT1, X_VEGLT1, X_RFDRHV5,
                                        HLTHPLN1, MEDCOST, GENHLTH, MENTHLTH, PHYSHLTH, DIFFWALK,
                                        SEX, X_AGEG5YR, EDUCA, INCOME2))



# IV.a.- Renombrar variables para facilitar la comprensión.

datos = rename(datos, c("Diabetes"=DIABETE3, "HighBloodPressure"=X_RFHYPE5, "HighChol"=TOLDHI2, "CholCheck"=X_CHOLCHK,
                        "BMI"=X_BMI5, "Smoker"=SMOKE100, "Stroke"=CVDSTRK3, "HeartDiseaseOrAttack"=X_MICHD, 
                        "PhysicalActivity"=X_TOTINDA, "Fruits"=X_FRTLT1, "Vegetables"=X_VEGLT1, "Alcoholic"=X_RFDRHV5,
                        "HealthCare"=HLTHPLN1, "NoDoctor"=MEDCOST, "GeneralHealth"=GENHLTH, "MentalHealth"=MENTHLTH,
                        "PhysicalHealth"=PHYSHLTH, "DifficultWalk"=DIFFWALK, "Sex"=SEX, "Age"=X_AGEG5YR, 
                        "Education"=EDUCA, "Income"=INCOME2))





# V.- Análisis inicial de los datos.

View(datos)
str(datos)
summary(datos)
sapply(datos,function(x) sum(is.na(x)))





# VI.- Limpieza de los datos (Eliminar missings).

datos <- na.omit(datos)





# VII.- Transformación de los datos.

# 0.- Diabetes. 

# Datos:
#   1 - diabético.
#   2 - diabetes gestacional.
#   3 - no diabético.
#   4 - cerca del límite de la diabetes.
#   7 - no lo sabe / no está seguro.
#   9 - no responde.

# Transformación:
#   0 - no diabético o diabetes gestacional.
#   1 - prediabético o cerca del límite de la diabetes. 
#   2 - diabético.
#   eliminar los que no lo saben / no están seguros o no responden.

table(datos$Diabetes)

datos$Diabetes <- replace(datos$Diabetes, datos$Diabetes==2, 0)
datos$Diabetes <- replace(datos$Diabetes, datos$Diabetes==3, 0)
datos$Diabetes <- replace(datos$Diabetes, datos$Diabetes==1, 2)
datos$Diabetes <- replace(datos$Diabetes, datos$Diabetes==4, 1)
datos$Diabetes <- replace(datos$Diabetes, datos$Diabetes==7, NA)
datos$Diabetes <- replace(datos$Diabetes, datos$Diabetes==9, NA)

datos <- na.omit(datos)

table(datos$Diabetes)



# 1.- Presión arterial.

# Datos:
#   1 - tiene tensión alta.
#   2 - no tiene tensión alta.
#   9 - no responde.

# Transformación:
#   0 - no tiene tensión alta.
#   1 - tiene tensión alta.
#   eliminar los que no responden.

table(datos$HighBloodPressure)

datos$HighBloodPressure <- replace(datos$HighBloodPressure, datos$HighBloodPressure==1, 0)
datos$HighBloodPressure <- replace(datos$HighBloodPressure, datos$HighBloodPressure==2, 1)
datos$HighBloodPressure <- replace(datos$HighBloodPressure, datos$HighBloodPressure==9, NA)

datos <- na.omit(datos)

table(datos$HighBloodPressure)



# 2.- Colesterol alto.

# Datos:
#   1 - sí tiene colesterol alto.
#   2 - no tiene colesterol alto.
#   7 - no lo sabe / no está seguro.
#   9 - no responde.

# Trasnformación:
#   0 - no tiene el colesterol alto.
#   1 - sí tiene el colesterol alto.
#   eliminar los que no lo saben / no están seguros o no responden.

table(datos$HighChol)

datos$HighChol <- replace(datos$HighChol, datos$HighChol==2, 0)
datos$HighChol <- replace(datos$HighChol, datos$HighChol==7, NA)
datos$HighChol <- replace(datos$HighChol, datos$HighChol==9, NA)

datos <- na.omit(datos)

table(datos$HighChol)



# 3.- Revisión del colesterol en los últimos 5 a?os.

#Datos:
#   1 - colesterol revisado en los últimos 5 a?os.
#   2 - colesterol no revisado en los últimos 5 a?os.
#   3 - colesterol no revisado en los últimos 5 a?os.
#   9 - no responde.

# Transformación
#   0 - colesterol no revisado en los últimos 5 a?os.
#   1 - colesterol revisado en los últimos 5 a?os.
#   eliminar los que no responden.

table(datos$CholCheck)

datos$CholCheck <- replace(datos$CholCheck, datos$CholCheck==2, 0)
datos$CholCheck <- replace(datos$CholCheck, datos$CholCheck==3, 0)
datos$CholCheck <- replace(datos$CholCheck, datos$CholCheck==9, NA)

datos <- na.omit(datos)

table(datos$CholCheck)



# 4.- Indice de masa muscular (IMC).

# Datos:
#   MBI * 100.

# Transformación:
#   % MBI: MBI/100.

str(datos$BMI)

datos$BMI <- replace(datos$BMI, TRUE, round(datos$BMI/100,2))

str(datos$BMI)



# 5.- Fumador.

# Datos:
#   1 - fumador.
#   2 - no fumador.
#   7 - no lo sabe / no está seguro.
#   9 - no responde.

# Transformación:
#   0 - no fumador.
#   1 - fumador.
#   eliminar los que no lo saben / no están seguros o no responden.

table(datos$Smoker)

datos$Smoker <- replace(datos$Smoker, datos$Smoker==2, 0)
datos$Smoker <- replace(datos$Smoker, datos$Smoker==7, NA)
datos$Smoker <- replace(datos$Smoker, datos$Smoker==9, NA)

datos <- na.omit(datos)

table(datos$Smoker)



# 6.- Ictus.

# Datos:
#   1 - sí ha padecido algún ictus.  
#   2 - no ha padecido ningún ictus.
#   7 - no lo sabe / no está seguro.
#   9 - no responde.

# Transformaci?n:
#   0 - no ha padecido ning?n ictus.
#   1 - s? ha padecido alg?n ictus.
#   eliminar los que no lo saben / no est?n seguros o no responden.

table(datos$Stroke)

datos$Stroke <- replace(datos$Stroke, datos$Stroke==2, 0)
datos$Stroke <- replace(datos$Stroke, datos$Stroke==7, NA)
datos$Stroke <- replace(datos$Stroke, datos$Stroke==9, NA)

datos <- na.omit(datos)

table(datos$Stroke)



# 7.- Enfermedad coronaria del coraz?n o infarto de miocardio.

# Datos:
#   1 - s? ha padecido enfermedad coronaria del coraz?n o infarto de miocardio.  
#   2 - no ha padecido enfermedad coronaria del coraz?n o infarto de miocardio.

# Transformaci?n:
#   0 - no ha padecido enfermedad coronaria del coraz?n o infarto de miocardio. 
#   1 - s? ha padecido enfermedad coronaria del coraz?n o infarto de miocardio.

table(datos$HeartDiseaseOrAttack)

datos$HeartDiseaseOrAttack <- replace(datos$HeartDiseaseOrAttack, datos$HeartDiseaseOrAttack==2, 0)

table(datos$HeartDiseaseOrAttack)



# 8.- Actividad f?sica.

# Datos:
#   1 - s? practica actividad f?sica.
#   2 - no practica actividad f?sica.
#   9 - no responde.

# Transformaci?n:
#   0 - no practica actividad f?sica. 
#   1 - s? practica actividad f?sica.
#   eliminar los que no responden.

table(datos$PhysicalActivity)

datos$PhysicalActivity <- replace(datos$PhysicalActivity, datos$PhysicalActivity==2, 0)
datos$PhysicalActivity <- replace(datos$PhysicalActivity, datos$PhysicalActivity==9, NA)

datos <- na.omit(datos)

table(datos$PhysicalActivity)



# 9.- Consumo de fruta.

# Datos:
#   1 - s? consume al menos una pieza de fruta al d?a.
#   2 - no consume al menos una pieza de fruta al d?a.
#   9 - no responde.

# Transformaci?n:
#   0 - no consume al menos una pieza de fruta al d?a.
#   1 - s? consume al menos una pieza de fruta al d?a.
#   eliminar los que no responden.

table(datos$Fruits)

datos$Fruits <- replace(datos$Fruits, datos$Fruits==2, 0)
datos$Fruits <- replace(datos$Fruits, datos$Fruits==9, NA)

datos <- na.omit(datos)

table(datos$Fruits)



# 10.- Consumo de verdura.

# Datos:
#   1 - s? consume al menos una raci?n de verdura al d?a.
#   2 - no consume al menos una raci?n de verdura al d?a.
#   9 - no responde.

# Transformaci?n:
#   0 - no consume al menos una raci?n de verdura al d?a.
#   1 - s? consume al menos una raci?n de verdura al d?a.
#   eliminar los que no responden.

table(datos$Vegetables)

datos$Vegetables <- replace(datos$Vegetables, datos$Vegetables==2, 0)
datos$Vegetables <- replace(datos$Vegetables, datos$Vegetables==9, NA)

datos <- na.omit(datos)

table(datos$Vegetables)



# 11.- Consumo excesivo de alcohol.

# Datos:
#   1 - no consume alcohol en exceso. 
#   2 - consume alcohol en exceso.
#   9 - no responde.

# Transformaci?n:
#   0 - no consume alcohol en exceso.  
#   1 - consume de alcohol en exceso.
#   eliminar los que no responden.

table(datos$Alcoholic)

datos$Alcoholic <- replace(datos$Alcoholic, datos$Alcoholic==1, 0)
datos$Alcoholic <- replace(datos$Alcoholic, datos$Alcoholic==2, 1)
datos$Alcoholic <- replace(datos$Alcoholic, datos$Alcoholic==9, NA)

datos <- na.omit(datos)

table(datos$Alcoholic)



# 12.- Acceso a atenci?n sanitaria.

# Datos:
#   1 - s? tiene acceso a atenci?n sanitaria.
#   2 - tiene acceso a atenci?n sanitaria.
#   7 - no lo sabe / no est? seguro.
#   9 - no responde.

# Transformaci?n:
#   0 - no tiene acceso a atenci?n sanitaria.
#   1 - s? tiene acceso a atenci?n sanitaria.
#   eliminar los que no saben / no est?n seguros o no responden.

table(datos$HealthCare)

datos$HealthCare <- replace(datos$HealthCare, datos$HealthCare==2, 0)
datos$HealthCare <- replace(datos$HealthCare, datos$HealthCare==7, NA)
datos$HealthCare <- replace(datos$HealthCare, datos$HealthCare==9, NA)

datos <- na.omit(datos)

table(datos$HealthCare)



# 13.- No ha podido visitar un m?dico en los ?ltimos 12 meses debido a los costes.

# Datos:
#   1 - no ha podido visitarlo.
#   2 - s? ha podido visitarlo.
#   7 - no lo sabe / no est? seguro.
#   9 - no responde.

# Transformaci?n:
#   0 - s? ha podido visitarlo.
#   1 - no ha podido visitarlo.
#   eliminar los que no saben / no est?n seguros o no responden.

table(datos$NoDoctor)

datos$NoDoctor <- replace(datos$NoDoctor, datos$NoDoctor==2, 0)
datos$NoDoctor <- replace(datos$NoDoctor, datos$NoDoctor==7, NA)
datos$NoDoctor <- replace(datos$NoDoctor, datos$NoDoctor==9, NA)

datos <- na.omit(datos)

table(datos$NoDoctor)



# 14.- Estado general de salud.

# Datos:
#   Escala del 1 (excelente) al 5 (pobre).
#   7 - no lo sabe / no est? seguro.
#   9 - no responde.

# Transformaci?n:
#   Escala del 1 (excelente) al 5 (pobre).
#   eliminar los que no saben / no est?n seguros o no responden.

table(datos$GeneralHealth)

datos$GeneralHealth <- replace(datos$GeneralHealth, datos$GeneralHealth==7, NA)
datos$GeneralHealth <- replace(datos$GeneralHealth, datos$GeneralHealth==9, NA)

datos <- na.omit(datos)

table(datos$GeneralHealth)



# 15.- N?mero de d?as del mes que ha tenido una mala salud mental.

# Datos:
#   Escala de d?as del 1 a 30.
#   77 - no lo sabe / no est? seguro.
#   88 - ning?n d?a.
#   99 - no responde.

# Transformaci?n:
#   0 - ning?n d?a.
#   Escala de d?as del 1 a 30.
#   eliminar los que no saben / no est?n seguros o no responden.

table(datos$MentalHealth)

datos$MentalHealth <- replace(datos$MentalHealth, datos$MentalHealth==88, 0)
datos$MentalHealth <- replace(datos$MentalHealth, datos$MentalHealth==77, NA)
datos$MentalHealth <- replace(datos$MentalHealth, datos$MentalHealth==99, NA)

datos <- na.omit(datos)

table(datos$MentalHealth)



# 16.- N?mero de d?as del mes que ha tenido una mala salud f?sica.

# Datos:
#   Escala de d?as del 1 a 30.
#   77 - no lo sabe / no est? seguro.
#   88 - ning?n d?a.
#   99 - no responde.

# Transformaci?n:
#   0 - ning?n d?a.
#   Escala de d?as del 1 a 30.
#   eliminar los que no saben / no est?n seguros o no responden.

table(datos$PhysicalHealth)

datos$PhysicalHealth <- replace(datos$PhysicalHealth, datos$PhysicalHealth==88, 0)
datos$PhysicalHealth <- replace(datos$PhysicalHealth, datos$PhysicalHealth==77, NA)
datos$PhysicalHealth <- replace(datos$PhysicalHealth, datos$PhysicalHealth==99, NA)

datos <- na.omit(datos)

table(datos$PhysicalHealth)



# 17.- Dificultad para caminar o subir escaleras.

# Datos:
#   1 - s?.
#   2 - no.
#   7 - no lo sabe / no est? seguro.
#   9 - no responde.

#Transformaci?n:
#   0 - no.
#   1 - s?.
#   eliminar los que no lo saben / no est?n seguros o no responden.

table(datos$DifficultWalk)

datos$DifficultWalk <- replace(datos$DifficultWalk, datos$DifficultWalk==2, 0)
datos$DifficultWalk <- replace(datos$DifficultWalk, datos$DifficultWalk==7, NA)
datos$DifficultWalk <- replace(datos$DifficultWalk, datos$DifficultWalk==9, NA)

datos <- na.omit(datos)

table(datos$DifficultWalk)



# 18.- Sexo.

# Datos:
#   1 - hombre.
#   2 - mujer.

# Transformaci?n:
#   0 - mujer.
#   1 - hombre.

table(datos$Sex)

datos$Sex <- replace(datos$Sex, datos$Sex==2, 0)

table(datos$Sex)



# 19.- Edad.

#Datos:
#   Intervalos de 5 a?os desde 18 a?os hasta m?s de 80 a?os.
#   14 - no lo sabe / no est? seguro o no responde.

#Transformaci?n:
#   Intervalos de 5 a?os desde 18 a?os hasta m?s de 80 a?os.
#   Eliminar los que no lo saben / no est?n seguros o no responde.


table(datos$Age)

datos$Age <- replace(datos$Age, datos$Age==14, NA)

datos <- na.omit(datos)

table(datos$Age)



# 20.- Educaci?n.

# Datos:
#   Escala del 1 (nunca ha ido al colegio) al 6 (ha ido a la universidad durante 4 a?os o m?s).
#   9 - no responde.

# Transformaci?n:
#   Escala del 1 (nunca ha ido al colegio) al 6 (ha ido a la universidad durante 4 a?os o m?s).
#   Eliminar los que no responden.

table(datos$Education)

datos$Education <- replace(datos$Education, datos$Education==9, NA)

datos <- na.omit(datos)

table(datos$Education)



# 21.- Ingresos.

# Datos:
# Escala del 1 (menos de $10,000) al 8 ($75,000 o m?s).
# 77 - no lo sabe / no est? seguro.
# 99 - no responde.

#Transformaci?n:
# Escala del 1 (menos de $10,000) al 8 ($75,000 o m?s).
# Eliminar los que no lo saben / no est?n seguros o no responden.

table(datos$Income)

datos$Income <- replace(datos$Income, datos$Income==77, NA)
datos$Income <- replace(datos$Income, datos$Income==99, NA)

datos <- na.omit(datos)

table(datos$Income)



# Punto de control.

datos_limpios <- datos



# 22.- Eliminamos los prediab?ticos o cercanos a ser diab?ticos, ya que estrictamente no pertenecen a ninguno 
#         de los dos grupos, y clasificarlos dentro de estos puede adulterar los resultados.

# Datos:
#   0 - no diab?tico o diabetes gestacional.
#   1 - prediab?tico o cerca del l?mite de la diabetes. 
#   2 - diab?tico.

# Transformaci?n:
#   0 - no diab?tico o diabetes gestacional.
#   1 - diab?tico.

table(datos$Diabetes)

datos$Diabetes <- replace(datos$Diabetes, datos$Diabetes==1, NA)
datos$Diabetes <- replace(datos$Diabetes, datos$Diabetes==2, 1)

datos <- na.omit(datos)

table(datos$Diabetes)
prop.table(table(datos$Diabetes))
hist(datos$Diabetes)





# VIII.- An?lisis exploratorio de los datos.

View(datos)
str(datos)
summary(datos)
sapply(datos,function(x) sum(is.na(x)))
describe(datos)



# VIII.a.- Matriz de correlaci?n para visualizar relaciones con la variable dependiente.

correlacion<-round(cor(datos), 1)
corrplot(round(cor(datos), 1), method="number", type="upper")

chart.Correlation(datos, histogram = F, pch = 19)





# IX.- Dise?o y desarrollo de modelos de aprendizaje supervisado (regresi?n lineal, ?rboles de decisi?n, random forest, 
#           m?quinas de soporte vectorial, naive bayes, redes neuronales...).

# IX.a.- Partici?n de los datos en los conjuntos de prueba y entrenamiento

set.seed(123)

particion <- sample.split(datos$Diabetes, SplitRatio = 0.75)
train <-subset(datos, particion==TRUE)
test <- subset(datos, particion==FALSE)

table(train$Diabetes)
prop.table(table(train$Diabetes))
hist(train$Diabetes)



# IX.b.- Creaci?n de datasets balanceados mediante la t?cnica de Undersampling. 

train_balanced <- ovun.sample(Diabetes ~ ., data = train, method = "under")$data

table(train_balanced$Diabetes)
prop.table(table(train_balanced$Diabetes))  



# IX.c.- Implementaci?n de modelos

# IX.c.1.- Regresi?n log?stica

# Dataset desbalanceado

logit_train <- glm(Diabetes~., data = train, family = "binomial")
logit_pred <- predict(logit_train, newdata = test, type = "response")
logit_pred_cod <- ifelse(logit_pred > 0.5, 1, 0)


# Dataset balanceado

logit_train_balanced <- glm(Diabetes~., data = train_balanced, family = "binomial")
logit_pred_balanced <- predict(logit_train_balanced, newdata = test, type = "response")
logit_pred_cod_balanced <- ifelse(logit_pred_balanced > 0.5, 1, 0)



# IX.c.2.- ?rbol de decisi?n

# Dataset desbalanceado

tree_train <- rpart(Diabetes~., data = train, method = "class")
tree_pred <- predict(tree_train, newdata = test, type = "class")


# Dataset balanceado

tree_train_balanced <- rpart(Diabetes~., data = train_balanced, method = "class")
tree_pred_balanced <- predict(tree_train_balanced, newdata = test, type = "class")



# IX.c.3.- Random Forest

# Dataset desbalanceado

randomForest_train <- ranger(Diabetes~., data = train, importance = "impurity")
randomForest_pred <- predict(randomForest_train, test, type = "response")
randomForest_pred_cod <- ifelse(randomForest_pred$predictions > 0.5, 1, 0)


# Dataset balanceado

randomForest_train_balanced <- ranger(Diabetes~., data = train_balanced, importance = "impurity")
randomForest_pred_balanced <- predict(randomForest_train_balanced, test, type = "response")
randomForest_pred_cod_balanced <- ifelse(randomForest_pred_balanced$predictions > 0.5, 1, 0)



# IX.c.4.- M?quinas de Soporte Vectorial (SVM)

# Dataset desbalanceado

svm_train <- svm(Diabetes ~ ., data = train, type = 'C-classification', kernel = 'linear')
svm_pred <- predict(svm_train, newdata = test)


# Dataset balanceado

svm_train_balanced <- svm(Diabetes ~ ., data = train_balanced, type = 'C-classification', kernel = 'linear')
svm_pred_balanced <- predict(svm_train_balanced, newdata = test)



# IX.c.5.- Clasificador Bayesiano (Naive Bayes)

# Dataset desbalanceado

naiveBayes_train <- naive_bayes(as.factor(Diabetes) ~ ., data = train)
naiveBayes_pred <- predict(naiveBayes_train, newdata = test)


# Dataset balanceado

naiveBayes_train_balanced <- naive_bayes(as.factor(Diabetes) ~ ., data = train_balanced)
naiveBayes_pred_balanced <- predict(naiveBayes_train_balanced, newdata = test)



# IX.c.6.- Redes neuronales

# Dataset desbalanceado

neuralNet_train <- neuralnet(Diabetes~., data = train, hidden = c(3,2), linear.output = FALSE, threshold = 0.01, stepmax = 1e7)
neuralNet_pred <- compute(neuralNet_train, test)
neuralNet_pred_cod <- ifelse(neuralNet_pred$net.result > 0.5, 1, 0)


# Dataset balanceado

neuralNet_train_balanced <- neuralnet(Diabetes~., data = train_balanced, hidden = c(3,2), linear.output = FALSE, threshold = 0.01, stepmax = 1e7)
neuralNet_pred_balanced <- compute(neuralNet_train_balanced, test)
neuralNet_pred_cod_balanced <- ifelse(neuralNet_pred_balanced$net.result > 0.5, 1, 0)





# X.- M?tricas y resultados 

# X.c.1.- Regresi?n log?stica

# Dataset desbalanceado

summary(logit_train)

confusionMatrix(as.factor(logit_pred_cod),as.factor(test$Diabetes), positive = "1", dnn = c("Predicción", "Actual"))

confusionMatrix(as.factor(logit_pred_cod),as.factor(test$Diabetes), positive = "1")$overall
confusionMatrix(as.factor(logit_pred_cod),as.factor(test$Diabetes), positive = "1")$byClass

plot(confusionMatrix(as.factor(test$Diabetes), as.factor(logit_pred_cod), positive = "1", 
                     dnn = c("Actual", "Predicción"))$table, main = "MC Regresión logística (unbalanced)")


# Dataset balanceado

summary(logit_train_balanced)

confusionMatrix(as.factor(logit_pred_cod_balanced),as.factor(test$Diabetes), positive = "1", dnn = c("Predicción", "Actual"))

confusionMatrix(as.factor(logit_pred_cod_balanced),as.factor(test$Diabetes), positive = "1")$overall
confusionMatrix(as.factor(logit_pred_cod_balanced),as.factor(test$Diabetes), positive = "1")$byClass

plot(confusionMatrix(as.factor(test$Diabetes), as.factor(logit_pred_cod_balanced), positive = "1", 
                     dnn = c("Actual", "Predicción"))$table, main = "MC Regresión logística (balanced)")



# X.c.2.- ?rbol de decisi?n

# Dataset desbalanceado

summary(tree_train)

confusionMatrix(as.factor(tree_pred),as.factor(test$Diabetes), positive = "1", dnn = c("Predicción", "Actual"))

confusionMatrix(as.factor(tree_pred),as.factor(test$Diabetes), positive = "1")$overall
confusionMatrix(as.factor(tree_pred),as.factor(test$Diabetes), positive = "1")$byClass

plot(confusionMatrix(as.factor(test$Diabetes), as.factor(tree_pred), positive = "1", 
                     dnn = c("Actual", "Predicción"))$table, main = "MC Árbol de decisión (unbalanced)")


# Dataset balanceado

summary(tree_train_balanced)

confusionMatrix(as.factor(tree_pred_balanced),as.factor(test$Diabetes), positive = "1", dnn = c("Predicción", "Actual"))

confusionMatrix(as.factor(tree_pred_balanced),as.factor(test$Diabetes), positive = "1")$overall
confusionMatrix(as.factor(tree_pred_balanced),as.factor(test$Diabetes), positive = "1")$byClass

plot(confusionMatrix(as.factor(test$Diabetes), as.factor(tree_pred_balanced), positive = "1", 
                     dnn = c("Actual", "Predicción"))$table, main = "MC Árbol de decisión (balanced)")



# X.c.3.- Random Forest

# Dataset desbalanceado

summary(randomForest_train)

confusionMatrix(as.factor(randomForest_pred_cod),as.factor(test$Diabetes), positive = "1", dnn = c("Predicción", "Actual"))

confusionMatrix(as.factor(randomForest_pred_cod),as.factor(test$Diabetes), positive = "1")$overall
confusionMatrix(as.factor(randomForest_pred_cod),as.factor(test$Diabetes), positive = "1")$byClass

plot(confusionMatrix(as.factor(test$Diabetes), as.factor(randomForest_pred_cod), positive = "1", 
                     dnn = c("Actual", "Predicción"))$table, main = "MC Bosque aleatorio (unbalanced)")


# Dataset balanceado

summary(randomForest_train_balanced)

confusionMatrix(as.factor(randomForest_pred_cod_balanced),as.factor(test$Diabetes), positive = "1", dnn = c("Predicción", "Actual"))

confusionMatrix(as.factor(randomForest_pred_cod_balanced),as.factor(test$Diabetes), positive = "1")$overall
confusionMatrix(as.factor(randomForest_pred_cod_balanced),as.factor(test$Diabetes), positive = "1")$byClass

plot(confusionMatrix(as.factor(test$Diabetes), as.factor(randomForest_pred_cod_balanced), positive = "1", 
                     dnn = c("Actual", "Predicción"))$table, main = "MC Bosque aleatorio (balanced)")



# X.c.4.- M?quinas de Soporte Vectorial (SVM)

# Dataset desbalanceado

summary(svm_train)

confusionMatrix(as.factor(svm_pred),as.factor(test$Diabetes), positive = "1", dnn = c("Predicción", "Actual"))

confusionMatrix(as.factor(svm_pred),as.factor(test$Diabetes), positive = "1")$overall
confusionMatrix(as.factor(svm_pred),as.factor(test$Diabetes), positive = "1")$byClass

plot(confusionMatrix(as.factor(test$Diabetes), as.factor(svm_pred), positive = "1", 
                     dnn = c("Actual", "Predicción"))$table, main = "MC SVM (unbalanced)")


# Dataset balanceado

summary(svm_train_balanced)

confusionMatrix(as.factor(svm_pred_balanced),as.factor(test$Diabetes), positive = "1", dnn = c("Predicción", "Actual"))

confusionMatrix(as.factor(svm_pred_balanced),as.factor(test$Diabetes), positive = "1")$overall
confusionMatrix(as.factor(svm_pred_balanced),as.factor(test$Diabetes), positive = "1")$byClass

plot(confusionMatrix(as.factor(test$Diabetes), as.factor(svm_pred_balanced), positive = "1", 
                     dnn = c("Actual", "Predicción"))$table, main = "MC SVM (balanced)")



# X.c.5.- Clasificador Bayesiano (Naive Bayes)

# Dataset desbalanceado

summary(naiveBayes_train)

confusionMatrix(as.factor(naiveBayes_pred),as.factor(test$Diabetes), positive = "1", dnn = c("Predicción", "Actual"))

confusionMatrix(as.factor(naiveBayes_pred),as.factor(test$Diabetes), positive = "1")$overall
confusionMatrix(as.factor(naiveBayes_pred),as.factor(test$Diabetes), positive = "1")$byClass

plot(confusionMatrix(as.factor(test$Diabetes), as.factor(naiveBayes_pred), positive = "1", 
                     dnn = c("Actual", "Predicción"))$table, main = "MC Naive Bayes (unbalanced)")


# Dataset balanceado

summary(naiveBayes_train_balanced)

confusionMatrix(as.factor(naiveBayes_pred_balanced),as.factor(test$Diabetes), positive = "1", dnn = c("Predicción", "Actual"))

confusionMatrix(as.factor(naiveBayes_pred_balanced),as.factor(test$Diabetes), positive = "1")$overall
confusionMatrix(as.factor(naiveBayes_pred_balanced),as.factor(test$Diabetes), positive = "1")$byClass

plot(confusionMatrix(as.factor(test$Diabetes), as.factor(naiveBayes_pred_balanced), positive = "1", 
                     dnn = c("Actual", "Predicción"))$table, main = "MC Naive Bayes (balanced)")



# X.c.6.- Redes neuronales 

# Dataset desbalanceado

summary(neuralNet_train)

confusionMatrix(as.factor(neuralNet_pred_cod),as.factor(test$Diabetes), positive = "1", dnn = c("Predicción", "Actual"))

confusionMatrix(as.factor(neuralNet_pred_cod),as.factor(test$Diabetes), positive = "1")$overall
confusionMatrix(as.factor(neuralNet_pred_cod),as.factor(test$Diabetes), positive = "1")$byClass

plot(confusionMatrix(as.factor(test$Diabetes), as.factor(neuralNet_pred_cod), positive = "1", 
                     dnn = c("Actual", "Predicción"))$table, main = "MC Red neuronal (unbalanced)")


# Dataset balanceado

summary(neuralNet_train_balanced)

confusionMatrix(as.factor(neuralNet_pred_cod_balanced),as.factor(test$Diabetes), positive = "1", dnn = c("Predicción", "Actual"))

confusionMatrix(as.factor(neuralNet_pred_cod_balanced),as.factor(test$Diabetes), positive = "1")$overall
confusionMatrix(as.factor(neuralNet_pred_cod_balanced),as.factor(test$Diabetes), positive = "1")$byClass

plot(confusionMatrix(as.factor(test$Diabetes), as.factor(neuralNet_pred_cod_balanced), positive = "1", 
                     dnn = c("Actual", "Predicción"))$table, main = "MC Red neuronal (balanced)")




  
