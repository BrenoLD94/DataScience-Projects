# https://www.kaggle.com/c/talkingdata-adtracking-fraud-detection/data

# objetivo: Prever se um clique é fraudulento ou não.

# ip: endereço IP do clique.
# app: ID do aplicativo para marketing.
# device: ID do tipo de dispositivo do celular do usuário (por exemplo, iphone 6 plus, iphone 7, huawei mate 7 etc.)
# os: ID da versão do telefone móvel do usuário
# channel: ID do canal do editor de anúncios para celular
# click_time: registro de data e hora do clique (UTC)
# attribute_time: se o usuário baixar o aplicativo depois de clicar em um anúncio, este é o horário do download do aplicativo
# is_attributed: o destino a ser previsto, indicando que o aplicativo foi baixado

setwd("./")

#========================================== CARREGANDO AS BIBLIOTECAS ==========================================

library(ggplot2)
library(data.table)
library(lubridate)
library(dplyr)
library(randomForest)
library(DMwR)
library(ROCR)

#========================================== DEFININDO AS FUNÇÕES ==========================================

# função que calcula a accurácia
accuracy <- function(real, predicted, type="C"){
  # classification
  if(type == "C"){
    S = round((sum(real == predicted)/length(real)), 4) * 100
    print(paste(toString(S), "%"))
  }
  else if(type == "R"){  
    # regression
    print("...")
  }
}

# funções para gerar as curvas ROC e a relação precisão e recall, respectivamente.
plot.roc.curve <- function(predictions, title.text){
  perf <- performance(predictions, "tpr", "fpr")
  plot(perf, col = "black", lty = 1, lwd = 2, 
       main = title.text, cex.main = 0.6, 
       cex.lab = 0.8, xaxs="i", yaxs="i")
  abline(0,1, col = "red")
  auc <- performance(predictions, "auc")
  auc <- unlist(slot(auc, "y.values"))
  auc <- round(auc,2)
  legend(0.4,0.4, legend = c(paste0("AUC: ",auc)), cex = 0.6, bty = "n", box.col = "white")
  
}

plot.pr.curve <- function(predictions, title.text){
  perf <- performance(predictions, "prec", "rec")
  plot(perf, col = "black", lty = 1, lwd = 2,
       main = title.text, cex.main = 0.6, cex.lab = 0.8, xaxs = "i", yaxs = "i")
}


#========================================== INICIANDO A ANÁLISE ==========================================
file = "./train_sample.csv"

df_fraude <- fread(file)

# análise exploratória

str(df_fraude)

summary(df_fraude)

sum(is.na(df_fraude))

# verificando a quantidade de valores únicos
unique(df_fraude$app)

unique(df_fraude$os)

unique(df_fraude$ip)

unique(df_fraude$channel)

unique(df_fraude$device)

unique(df_fraude$attributed_time)

# existe muitos valores 1's
df_temp <- df_fraude %>%
  group_by(device) %>%
  summarise(quantidade = n()) %>%
  mutate(freq = round(quantidade/dim(df_fraude)[1], 2))

df_temp[order(df_temp$freq, decreasing = T), ]

#  tabela cruzada entre as variáveis device e is_attributed
deviceTable <- table(df_fraude$is_attributed, df_fraude$device)
names(dimnames(deviceTable)) <- c("Is_Attributed", "Device")
deviceTable

# transformando as variáveis para os tipos corretos
df_fraude$is_attributed <- as.factor(df_fraude$is_attributed)

df_fraude$click_time <- parse_date_time(df_fraude$click_time, orders="ymd HMS")

summary(df_fraude)

# engenharia de atributos
df_fraude$days <- weekdays(df_fraude$click_time)
df_fraude$days <- as.factor(sapply(df_fraude$days, 
                                   function(x){
                                     if(x == "segunda")
                                       return(1)
                                     else if(x == "terça")
                                       return(2)
                                     else if(x == "quarta")
                                       return(3)
                                     else if(x == "quinta")
                                       return(4)
                                     else if(x == "sexta")
                                       return(5)
                                     else if(x == "sábado")
                                       return(6)
                                     else if(x == "domingo")
                                       return(7)
                                   }))

df_fraude$clkHour <- hour(df_fraude$click_time)

df_fraude$device <- as.factor(sapply(df_fraude$device, 
                                     function(x){
                                       # not 1
                                       if(x != 1)
                                         return(2)
                                       else 
                                         return(1)
                                     }))


# CLICK POR APP DADO UM IP POR HORA 
df_fraude2 <- df_fraude %>%
  select(-c("attributed_time")) %>%
  group_by(ip, app, clkHour) %>%
  mutate(n = n()) %>%
  rename("IpAppClickByHour" = n)


# CLICK POR OS DADO UM IP POR HORA
df_fraude2 <- df_fraude2 %>%
  group_by(ip, os, clkHour) %>%
  mutate(n = n()) %>%
  rename("IpOsClickByHour" = n)


# CLICK POR CHANNEL DADO UM IP POR HORA
df_fraude2 <- df_fraude2 %>%
  group_by(ip, channel, clkHour) %>%
  mutate(n = n()) %>%
  rename("IpChannelClickByHour" = n)


# CLICK POR DIA DADO UM IP
df_fraude2 <- df_fraude2 %>%
  group_by(ip, days) %>%
  mutate(n = n()) %>%
  rename("IpClickByDay" = n)

df_fraude3 <- df_fraude2 %>%
  select(-c("ip", "app", "channel", "os"))
df_fraude3$ip <- NULL


# dataset totalmente desbalanceado
ggplot(data = df_fraude, aes(x = is_attributed)) +
  geom_histogram(stat="count")

# seleção de atributos
modeloImp1 <- randomForest(data = df_fraude,
                           is_attributed ~ .
                           -attributed_time
                           -ip,
                           ntree = 70, 
                           nodesize = 10,
                           importance = TRUE)

modeloImp2 <- randomForest(data = df_fraude2,
                           is_attributed ~ .
                           -ip,
                           ntree = 70, 
                           nodesize = 10,
                           importance = TRUE)

modeloImp3 <- randomForest(data = df_fraude3,
                           is_attributed ~ .,
                           ntree = 70, 
                           nodesize = 10,
                           importance = TRUE)

varImpPlot(modeloImp1)
varImpPlot(modeloImp2)
varImpPlot(modeloImp3)

df_fraude2$click_time <- NULL
df_fraude2$ip <- NULL

# balanceando os dados
df_fraudeBalanceado <- SMOTE(is_attributed ~ .,
                             data = as.data.frame(df_fraude2),
                             perc.over = 450,
                             perc.under = 150)

ggplot(data = df_fraudeBalanceado, aes(x = is_attributed)) +
  geom_histogram(stat="count")

# criação do modelo
set.seed(101)
indices <- sample(1:nrow(df_fraudeBalanceado), 0.7*nrow(df_fraudeBalanceado))

treino <- df_fraudeBalanceado[indices, ]
teste <- df_fraudeBalanceado[-indices, ]

varAlvo <- teste$is_attributed
teste$is_attributed <- NULL


# execução do modelo usando random forest
rf_model <- randomForest(is_attributed ~ .,
                         data = treino,
                         ntree = 90)

rf_prediction <- predict(rf_model, teste)
accuracy(varAlvo, rf_prediction)

# execução do modelo usando naive bayes
library(e1071)
naive_model <- naiveBayes(is_attributed ~.,
                          data = treino)


naive_prediction <- predict(naive_model, teste)
accuracy(varAlvo, naive_prediction)

# avaliação do modelo
library(caret)

tabelaContigencia <- table(rf_prediction, varAlvo)
names(dimnames(tabelaContigencia)) <- c("Previsto", "Real")

confusionMatrix(tabelaContigencia, positive = '1')

plot.roc.curve(prediction(predictions = as.numeric(rf_prediction), labels = as.numeric(varAlvo)), 
               "ROC CURVE")

plot.pr.curve(prediction(predictions = as.numeric(rf_prediction), labels = as.numeric(varAlvo)), 
              "PRECISION/RECALL")