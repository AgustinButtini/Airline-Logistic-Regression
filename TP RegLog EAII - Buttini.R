library(readxl)
library(ggplot2)
library(caret)

datos_compania <- 
  read_excel("D:/fiuba/Estadística Aplicada II/TP Regresión Logística/datos_104355_BUTTINI_AGUSTÍN.xlsx",
             sheet = "Datos")

#
#---------------punto 1---------------
#

factor_clase <- factor(datos_compania$clase,levels=c("Eco","Eco Plus","Business"), order = TRUE) 
datos_compania$clase_ordinal <- factor_clase 
factor_lealtad <- factor(datos_compania$lealtad,levels=c("Baja Lealtad","Alta Lealtad"), order = TRUE) 
datos_compania$lealtad_ordinal <- factor_lealtad

m1 <- glm(formula = as.factor(satisfecho) ~ . -clase -lealtad,
          data = datos_compania,
          family = binomial)
summary(m1)


ggplot(datos_compania) +
  geom_hline(yintercept = 0:1, color = "black")+
  geom_point(aes(x = sexo, y = as.numeric(as.factor(satisfecho))-1)) +
  geom_jitter(aes(x = sexo, y = as.numeric(as.factor(satisfecho))-1),
              width = 0.3, height  = 0.3,
              alpha = 0.5)+
  labs(y= "Satisfacción", x = "Sexo")+
  theme_bw()

ggplot(datos_compania) +
  geom_hline(yintercept = 0:1, color = "black")+
  geom_point(aes(x = tipo_viaje, y = as.numeric(as.factor(satisfecho))-1)) +
  geom_jitter(aes(x = tipo_viaje, y = as.numeric(as.factor(satisfecho))-1),
              width = 0.4, height  = 0.4,
              alpha = 0.5)+
  labs(y= "Satisfacción", x = "Tipo de Viaje")+
  theme_bw()

ggplot(datos_compania) +
  geom_hline(yintercept = 0:1, color = "black")+
  geom_point(aes(x = clase_ordinal, y = as.numeric(as.factor(satisfecho))-1)) +
  geom_jitter(aes(x = clase_ordinal, y = as.numeric(as.factor(satisfecho))-1),
              width = 0.4, height  = 0.3,
              alpha = 0.5)+
  labs(y= "Satisfacción", x = "Clase")+
  theme_bw()
#
#---------------punto 2---------------
#

mvacio <- glm(as.factor(satisfecho) ~ 1,
              data = datos_compania,
              family = binomial)
variables <- formula(as.factor(satisfecho) ~ sexo + lealtad_ordinal + edad + tipo_viaje + 
                       clase_ordinal + distancia + min_demora_partida + minutos_demora_arribo)

mstepf <- stepAIC(mvacio, trace=TRUE, direction="forward", scope=variables)

mstepb <- stepAIC(m1, trace=TRUE, direction="backward", scope=variables)

summary(mstepf)
summary(mstepb)


#
#---------------punto 3---------------
#

distancia_promedio <- mean(datos_compania$distancia)
print(distancia_promedio)
datos_compania$distancia_promedio <- distancia_promedio

m3 <- glm(formula = as.factor(satisfecho) ~ clase_ordinal + distancia_promedio + minutos_demora_arribo,
          data = datos_compania,
          family = binomial)
summary(m3)


datos_compania$predict_z <- predict(m3)
datos_compania$predict_p <- 1/(1+exp(-datos_compania$predict_z))

#se agrega un registro de p=0 donde minutos de demora = 800

clase_eco <- datos_compania%>% filter(clase_ordinal=="Eco")
clase_eco <- rbind(clase_eco, c("no",
                                "-",
                                "-",
                                0,
                                "-",
                                "-",
                                1978.39070351759,
                                800,
                                800,
                                "Eco",
                                "Baja Lealtad",
                                1978.39070351759,
                                -10,
                                0))


clase_ecoplus <- datos_compania%>% filter(clase_ordinal=="Eco Plus")
clase_ecoplus <- rbind(clase_ecoplus, c("no",
                                        "-",
                                        "-",
                                        0,
                                        "-",
                                        "-",
                                        1978.39070351759,
                                        800,
                                        800,
                                        "Eco Plus",
                                        "Baja Lealtad",
                                        1978.39070351759,
                                        -10,
                                        0))

clase_business <- datos_compania%>% filter(clase_ordinal=="Business")
clase_business <- rbind(clase_business, c("no",
                                          "-",
                                          "-",
                                          0,
                                          "-",
                                          "-",
                                          1978.39070351759,
                                          800,
                                          800,
                                          "Business",
                                          "Baja Lealtad",
                                          1978.39070351759,
                                          -10,
                                          0))
#GRÁFICO CON LAS 3 CLASES
ggplot(datos_compania, aes(x,y,col=clase_ordinal)) +
  geom_hline(yintercept = 0:1, color = "black")+
  xlim(0,800)+
  geom_jitter(aes(x = as.numeric(minutos_demora_arribo), y = as.numeric(as.factor(satisfecho))-1),
              width = 0, height  = 0,
              alpha = 0.5)+
  geom_line(data=clase_eco,aes(x = as.numeric(minutos_demora_arribo), y = as.numeric(predict_p)))+
  geom_line(data=clase_ecoplus,aes(x = as.numeric(minutos_demora_arribo), y = as.numeric(predict_p)))+
  geom_line(data=clase_business,aes(x = as.numeric(minutos_demora_arribo), y = as.numeric(predict_p)))+
  labs(y= "Satisfacción", x = "Demora en el arribo (minutos)")+
  ggtitle("Satisfacción en función de la Demora")+
  theme_bw()

#CLASES POR SEPARADO
ggplot(clase_eco) +
  geom_hline(yintercept = 0:1, color = "black")+
  xlim(0,800)+
  geom_jitter(aes(x = as.numeric(minutos_demora_arribo), y = as.numeric(as.factor(satisfecho))-1),
              width = 0.02, height  = 0.02,
              alpha = 0.5)+
  geom_line(aes(x = as.numeric(minutos_demora_arribo), y = as.numeric(predict_p)), col = 'blue')+
  labs(y= "Satisfacción", x = "Demora en el arribo (minutos)")+
  theme_bw()

ggplot(clase_ecoplus) +
  geom_hline(yintercept = 0:1, color = "black")+
  xlim(0,800)+
  geom_jitter(aes(x = as.numeric(minutos_demora_arribo), y = as.numeric(as.factor(satisfecho))-1),
              width = 0.02, height  = 0.02,
              alpha = 0.5)+
  geom_line(aes(x = as.numeric(minutos_demora_arribo), y = as.numeric(predict_p)), col = 'blue')+
  labs(y= "Satisfacción", x = "Demora en el arribo (minutos)")+
  theme_bw()

ggplot(clase_business) +
  geom_hline(yintercept = 0:1, color = "black")+
  xlim(0,800)+
  geom_jitter(aes(x = as.numeric(minutos_demora_arribo), y = as.numeric(as.factor(satisfecho))-1),
              width = 0.02, height  = 0.02,
              alpha = 0.5)+
  geom_line(aes(x = as.numeric(minutos_demora_arribo), y = as.numeric(predict_p)), col = 'blue')+
  labs(y= "Satisfacción", x = "Demora en el arribo (minutos)")+
  theme_bw()

#
#---------------punto 4---------------
#

#4a
set.seed(123)
random_sample <- createDataPartition(y = as.numeric(as.factor(datos_compania$satisfecho)),
                                     p = 0.75,
                                     list = FALSE)

training_dataset <- datos_compania[random_sample,]
testing_dataset <- datos_compania[-random_sample,]

m4<- glm(formula= as.factor(satisfecho) ~ clase_ordinal + distancia + minutos_demora_arribo,
         data=training_dataset,
         family=binomial)
summary(m4)

#4b
predict_z <- predict(m4, newdata = testing_dataset)
predict_p <- 1/(1+exp(-predict_z))

testing_dataset$predict_p <- predict_p


ggplot(testing_dataset, aes(x=predict_p)) + 
  geom_histogram(color="black", fill="blue", binwidth=0.02)+
  labs(y= "Densidad", x = "Probabilidad Testing")+
  xlim(0,0.8)+
  theme_bw()

#4c
predicted <- as.factor(ifelse(predict_p > 0.6, 1, 0))
actual <- as.factor(ifelse(testing_dataset$satisfecho=="si",1,0))

confusionMatrix(data = predicted,
                reference = actual)

