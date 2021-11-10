setwd("C:/Users/Computador/Desktop/PFRAUDE")

### CARREGANDO ARQUIVOS ###
df <- read.csv('train_sample.csv')
head(df)
str(df)
View(df)

summary (df)

###ANALISES DECSRITIVA DAS VARIÁVEIS###
summary(train_sample$ip)
summary(train_sample$app)
summary(train_sample$device)
summary(train_sample$os)
summary(train_sample$channel)

###VERIFICANDO SE HÁ DADOS FALTANTES ###
is.na(train_sample)
any(is.na(train_sample))

###TRATANDO O BANCO - VERIFICANDO MISSING###
df1 <-na.omit(df)
nrow(df)
nrow(df1)
NAS <- round(colSums(is.na(df))*100/nrow(df),2)
NAS
df_rf$attributed_time = NULL
df_rf$click_time = NULL
str(df_rf)

###CONVERSÃO DA VARIÁVEL TARGET PARA FATOR
df$is_attributed <- as.factor(df$is_attributed)

#PROSSEGUINDO NA ANÁLISE EXPLORATÓRIA
library(ggplot2)
library(dplyr)

###PROPORÇÃO EM CLASSES###
ggplot(df,aes(x=is_attributed)) +
geom_bar()

table(df$is_attributed)

table(df$is_attributed)[1]/nrow(df)
table(df$is_attributed)[2]/nrow(df)

###CLASSES DESBALANCEADAS - FRAUDES ### EM SEGUIDA BALANCEANDO CLASSES ### CARREGAR CSV ###

smoted_df <- read.csv('smoted_df.csv')

### REDUZIR NÚMERO DE CLASSES = 0 ###
smoted_df_sample <- smoted_df %>% filter(is_attributed == 0) %>% 
sample_n(3000)

### UNINDO LINHA QUE O RESULTADOS DA VARIÁVEL TARGET = 1 ###
smoted_df_sample <- rbind(smoted_df_sample,smoted_df[smoted_df$is_attributed==1,])
table(smoted_df_sample$is_attributed)
smoted_df_sample$is_attributed = as.factor(smoted_df_sample$is_attributed)



####CARREGANDO PACOTES - caret & randomForest###









###DIVIDINDO DADOS EM TESTE E TREINO EM 75% & 25%###
library(caret)
amostra <- createDataPartition(smoted_df_sample$is_attributed,p= 0.75,list=FALSE)
testData <- smoted_df_sample[amostra,]
trainData <- smoted_df_sample[-amostra,]

### PROPORÇÃO DAS CLASSES ###
table(testData$is_attributed)
table(trainData$is_attributed)


df_rf <- smoted_df_sample








#### MODELO 1 - RANDOM FOREST ####

library(randomForest)
modelo_rf_v1 <- randomForest(is_attributed ~ .,data=df_rf)
summary(modelo_rf_v1)
predictions <- predict(modelo_rf_v1, testData)

mean(predictions == testData$is_attributed)



### MODELO 2 - RADOM FOREST ###
modelo_rf_v2 <- randomForest(is_attributed ~ .,data=df_rf2)
summary(modelo_rf_v2)
predictions <- predict(modelo_rf_v2, testData)

mean(predictions == testData$is_attributed)






### Curva AUC -  avaliação do Machine Learning ###
library(pROC)
###Instalar ROCR e carregar###
library(ROCR)
roc_obj <- roc(testData$is_attributed, as.numeric(predictions))
roc_obj



###AUC ###
roc_obj <- roc(testData$is_attributed, as.numeric(predictions))
roc_obj



######### MODELO 1  --> 0.9847199, MODELO 2 -->  0.974048, Area under the curve: 0.974 ###
###MODELO 1 MELHOR ###







