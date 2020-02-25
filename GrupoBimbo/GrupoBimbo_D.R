# https://www.kaggle.com/c/grupo-bimbo-inventory-demand/data

# Objetivo: Prever quantos paẽs serão necessários serem produzidos para suprir as demandas dos clientes.

# ======================================== INÍCIO DO CÓDIGO ==============================================

setwd("./")

# carregando as bibliotecas
library(data.table)
library(ggplot2)
library(stringr)
library(dplyr)
library(corrplot)
library(randomForest)
library(caret)
library(MLmetrics)

# CARREGANDO ARUIVO EXTERNO
source('functions.R')

# ================================== ORGANIZANDO OS DADOS ==================================
path <- "../Dados/"

# carregando so dados
cliente <- fread(paste(path, "Client_Table.csv", sep=''))

amostra_treino <- fread(paste(path, "amostra.csv", sep=''))
amostra_treino$V1 <- NULL

# Há clientes com mesmo nome e ID's diferente caso eu exclua a linha provavelmente eu terei um valor
# NA nos dados de treino quando fizer o merge. Estaria jogando informação "fora"
length(unique(str_replace_all(cliente$NombreCliente, " ", "")))
length(unique(cliente$Cliente_ID))

produto <- read.csv(paste(path, "Product_Table.csv", sep=''))

cidade_estado <- read.csv(paste(path, "town_state.csv", sep=''))

# verifica se há valores nulos
sum(is.na(amostra_treino))

View(head(amostra_treino, 1000))

# Join dos dataframes pelos ID's
amostra <- merge(amostra_treino, cliente, by="Cliente_ID")

amostra <- merge(amostra, produto, by="Producto_ID")

amostra <- merge(amostra, cidade_estado, by="Agencia_ID")

# renomeando as colunas
colunas <- c("Agency_ID", "Product_ID", "Client_ID", "Week", "Channel_ID" , "Route_ID", 
             "SaleUnitByWeek", "SaleByWeek", "ReturnUnitWeek", "ReturnWeek", "adjRequest", 
             "ClientName", "ProductName", "City", "State")

names(amostra) <- colunas

View(head(amostra, 1000))

sum(is.na(amostra))

amostra$ClientName <- str_replace_all(amostra$ClientName, " ", "")

amostra <- as.data.frame(amostra)

# salvando as modificações em um csv
# write.csv(amostra, "../Dados/amostra.csv", row.names = FALSE)

# ================================== ANÁLISE EXPLORATÓRIA ==================================
str(amostra)

summary(amostra)

# NOIDENTIFICADO e SINNOMBRE no final representam a mesma ideia
sum(amostra$ClientName == "NOIDENTIFICADO")
sum(amostra$ClientName == "SINNOMBRE")

# verificando outliers
numeric_vars <- c("ReturnUnitWeek", "ReturnWeek", "SaleUnitByWeek", "SaleByWeek", "adjRequest",
                  "Route_ID", "Channel_ID", "Week")

# muitos outliers
for(col in numeric_vars){
  g <- boxplot(amostra[, col])
  title(paste("Boxplot", col, sep=" "))
}

for(col in numeric_vars){
  g1 <- hist(amostra[, col], xlab = col)
}

View(arrange(amostra, desc(adjRequest)))

# correlação
corr <- cor(amostra[, numeric_vars])
corrplot(corr, type = "lower", method = "circle", order = "FPC", addCoef.col = "black")

# ================================== TRANSFORMAÇÃO DOS DADOS ==================================
# engenharia de atributos
amostra$Peso <- sapply(as.character(amostra$ProductName), 
                            function(x){
                              temp = unlist(str_extract_all(as.character(x), "\\d+g|\\d+Kg|\\d+kg|\\d+KG"))
                              if(length(temp) > 0)
                                return(as.character(temp))
                              else
                                return("0g")
                            })

amostra$Peso <- sapply(amostra$Peso, 
       function(x){
         if(str_detect(x, "\\d+KG|\\d+kg|\\d+Kg")){
           temp = str_replace_all(x, "kg|KG|Kg", "")
           temp = as.integer(temp)*1000
         }
         else{
           temp = as.integer(str_replace_all(x, "g|G", ""))
         }
         return(temp)
       })

# Transformando as variáveis para o tipo mais correto
amostra$Week <- as.factor(amostra$Week)
amostra$Channel_ID <- as.factor(amostra$Channel_ID)

numeric_vars2 <- c("ReturnUnitWeek", "ReturnWeek", "SaleUnitByWeek", "SaleByWeek", "adjRequest",
                  "Route_ID", "Peso")

corr2 <- cor(amostra[, numeric_vars2])
corrplot(corr2, type = "lower", method = "circle", order = "FPC", addCoef.col = "black")

# Inserindo a coluna região baseado nos estados do méxico
amostra$State <- sapply(as.character(amostra$State), 
       function(x){
         if(x == "Queretaro de Arteaga")
           return("QUERETARO")
         else
           return(x)
       })

amostra$Regiao <- as.factor(sapply(amostra$State, getRegiao))

# classificando os produtos por categorias
temp <- str_replace_all(str_replace(amostra$ProductName, "\\s", "-"), " .*", "")
temp <- str_replace_all(amostra$ProductName, " .*", "")
temp <- as.factor(temp)

amostra$CategoriaProduto <- sapply(temp, 
       function(x){
         if(str_detect(x, "Pan|Medias|Bimbollos"))
           return("Pao Comum")
         else if(str_detect(x, "Roles|Rebanada|Colchones"))
           return("Pao Doce")
         else if(str_detect(x, "Mantecadas|Panque|Donas|Donitas|Submarinos|Madalenas|Pinguinos|Panquecito|Dalmata|Bollo|Bollos"))
           return("Bolinhos")
         else if(str_detect(x, "Nito|Barritas|Gansito|Principe|Bimbunuelos|Conchas|Choco|Sponch"))
           return("Doces")
         else if(str_detect(x, "Triki|Canelitas|Suavicremas|Tostado|Bran|Tostada|Cuernitos"))
           return("Pao Doce")
         else
           return("Diversos")
       })

amostra$CategoriaProduto <- as.factor(amostra$CategoriaProduto)


# salvando as modificações em um csv
# write.csv(amostra, "../Dados/amostra2.csv", row.names = FALSE)

# tratando outliers
# coluna adjRequest
View(arrange(amostra, desc(adjRequest)))

boxplot(amostra$adjRequest)

Q <- quantile(amostra$adjRequest, probs=c(.25, .75))
iqr <- IQR(amostra$adjRequest)

# usando a formula chego que aproximadamente 10% dos dados são outliers
amostra %>%
  filter(adjRequest >= Q[2]+1.5*iqr) %>%
  summarise(count = n()/dim(amostra)[1])

amostra %>%
  filter(adjRequest >= 30) %>%
  summarise(count = n())

boxplot(amostra$adjRequest)
hist(amostra$adjRequest)

# coluna SaleByWeek
View(arrange(amostra, desc(SaleByWeek)))

amostra %>%
  filter(SaleByWeek >= 500) %>%
  summarise(count = n())

# coluna ReturnWeek
View(arrange(amostra, desc(ReturnWeek)))

amostra %>%
  filter(ReturnWeek >= 80) %>%
  summarise(count = n())

amostra2 <- subset(amostra, adjRequest <= 30)
amostra2 <- subset(amostra2, SaleByWeek <= 500)
amostra2 <- subset(amostra2, ReturnWeek <= 80)

# Tratando as colunas numericas
numeric_vars3 <- c("ReturnUnitWeek", "ReturnWeek", "SaleUnitByWeek", "SaleByWeek", "adjRequest",
                  "Peso")

# como p < 0.05, então rejeito à hipótese nula, ou seja, os dados não são normais
nRow <- nrow(amostra2)
for(coluna in numeric_vars3){
  print(paste(coluna, ": ", sep=''))
  print(shapiro.test(amostra2[sample(1:nRow, 5000), coluna]))
}

amostra2$Regiao <- as.factor(amostra2$Regiao)
amostra2$CategoriaProduto <- as.factor(amostra2$CategoriaProduto)

amostra3 <- amostra2

for(coluna in numeric_vars3){
  amostra3[, coluna] <- normalization(amostra3[, coluna])  
}

amostra3 <- amostra3 %>%
            select(-c('Agency_ID', "Product_ID", "Client_ID", "Route_ID", "City", "ProductName", "State", 
                      "ClientName"))
  
# ================================== SELEÇÃO DE ATRIBUTOS ==================================
subAmostra2 <- amostra2[sample(1:nrow(amostra2), 100000), ]
subAmostra3 <- amostra3[sample(1:nrow(amostra3), 100000), ]
  

rf_model_signif <- randomForest( adjRequest ~.
                             -Agency_ID
                             -Product_ID
                             -Client_ID
                             -Route_ID
                             -City
                             -ProductName
                             -State
                             -ClientName,
                             data = subAmostra2,
                             ntree = 30,
                             nodesize = 10,
                            importance = TRUE)

rf_model_signif2 <- randomForest( adjRequest ~.,
                                 data = subAmostra3,
                                 ntree = 30,
                                 nodesize = 10,
                                 importance = TRUE)

varImpPlot(rf_model_signif)
varImpPlot(rf_model_signif2)

# vou retirar channel_ID e Week, por apresentarem baixa significância
colunas_mais_Signif <- c("SaleUnitByWeek", "SaleByWeek", "ReturnUnitWeek", "ReturnWeek",
                         "Peso", "Regiao", "CategoriaProduto", "adjRequest")


# ================================== DADOS DE TREINO E TESTE =============================
set.seed(101)

indices <- sample(1:nrow(amostra3), 0.7*nrow(amostra3))

treino_amostra3 <- amostra3[indices, colunas_mais_Signif]
test_amostra3 <- amostra3[-indices, colunas_mais_Signif]

treino_amostra2 <- amostra2[indices, colunas_mais_Signif]
test_amostra2 <- amostra2[-indices, colunas_mais_Signif]

# ================================== CRIAÇÃO DO MODELO =============================
# amostra3 é normalizada, já a amostra2 não é.
# amostra3 - knn
knn_model <- knnreg(formula = adjRequest ~ ., 
                   data = treino_amostra3,
                   k = 5)

knn_predict <- predict(knn_model, test_amostra3[, 1:7])

knn_result <- data.frame(previsto = knn_predict, real = test_amostra3[, 8])

residuo <- (knn_result$previsto - knn_result$real)

mean(residuo)

# amostra3 - lm
lm_model <- lm(formula = adjRequest ~.,
               data = treino_amostra3)

lm_predict <- predict(lm_model, test_amostra3[, 1:7])

lm_result <- data.frame(previsto = lm_predict, real = test_amostra3[, 8])

# amostra2
knn_model2 <- knnreg(formula = adjRequest ~ ., 
                    data = treino_amostra2,
                    k = 4)

knn_predict2 <- predict(knn_model2, test_amostra2[, 1:7])

knn_result2 <- data.frame(previsto = knn_predict2, real = test_amostra2[, 8])

residuo2 <- (knn_result2$previsto - knn_result2$real)

# ================================== AVALIANDO O MODELO =============================
# quanto mais próximo de 1 melhor é o modelo

# amostra3 - KNN
R2_Score(y_pred = knn_result$previsto, y_true = knn_result$real)

# amostra3 - LM
R2_Score(y_pred = lm_result$previsto, y_true = lm_result$real)

# amostra2
R2_Score(y_pred = knn_result2$previsto, y_true = knn_result2$real)

# Tanto o KNN quanto o LM apresentaram resultados bem altos e similares, mas o KNN demorou
# mais para treinar o modelo.

# ================================== FIM ===========================================
