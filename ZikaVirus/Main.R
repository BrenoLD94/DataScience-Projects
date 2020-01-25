# Origem do dados: https://github.com/cdcepi/zika
# dicionário dos dados: https://github.com/cdcepi/zika/blob/master/data_dictionary.md

setwd("./")

source("./functions.R")

library(dplyr)
library(ggplot2)
library(stringr)
library(lubridate)
library(tidyr)

files <- list.files("./Dados", pattern = "*.csv")

zika <- (lapply(paste("./Dados/", files, sep=''), read.csv, stringsAsFactors = FALSE))

zika <- joinByRowDataF(zika)

# explorando e organizando os dados
str(zika)

summary(zika)

sapply(zika, 
       function(x){
         sum(is.na(x))
       })

sapply(zika, 
       function(x){
         unique(x)
       })


zika <- deleteColumns(zika, c("time_period", "time_period_type"))

zika$location <- sapply(zika$location, function(x, pattern = "Brazil-", novoTexto = ""){
  return(str_replace(x, pattern = pattern, novoTexto))
})

zika$report_date <- as.Date(zika$report_date)

zika$Dia <- day(zika$report_date)
zika$Mes <- month(zika$report_date)
zika$Ano <- year(zika$report_date)


zika <- zika %>%
  select(c(-4, -5, -7))

# plots para analisar os dados
zika2 <- zika %>%
  select(c(report_date, location, location_type, value, Mes)) %>%
  filter(location_type == "region") %>%
  group_by(location, Mes) %>%
  summarise(casos = sum(value))

g1 <- ggplot(data = zika2, aes(x = as.factor(location), y = casos, fill = as.factor(Mes))) +
  geom_bar(stat = "identity", position = 'dodge') +
  ggtitle("Número de casos de Zika por Região de 2016") +
  xlab("Localização") + 
  ylab("Número de casos") +
  labs(fill = "Mês") +
  scale_fill_manual(values=c("#4169E1", "#191970",  "#4682B4"),  labels = c("Abril", "Maio", "Junho"))

# censo: https://www.ibge.gov.br/estatisticas/sociais/populacao/9662-censo-demografico-2010.html?edicao=9673&t=downloads
populacao2010 <- data.frame(Regiao = c("Sul", "Centro_Oeste", "Sudeste", "Norte", "Nordeste"),
                            Populacao = c(27386891, 14058094, 80364410, 15864454, 53081950))

color <- c("blue", "yellow", "purple", "green", "orange")

g2 <- ggplot(data = populacao2010, aes(x = Regiao, y = Populacao/1000000)) +
  geom_bar(stat = "identity", position = 'dodge', fill = color) +
  ggtitle("Número de Pessoas por Região em 2010") +
  xlab("Localização") + 
  ylab("População(em milhões)") 

library(readxl)

# pessoas acima de 10 anos de idade por grau de instrução
escolaridade <- read_xls("./Dados/EscolaridadeCenso2010.xls")

escolaridade$Total <- NULL

escolaridade2 <- escolaridade %>%
  filter(Localizacao %in% c("Norte", "Sudeste", "Centro-Oeste", "Nordeste", "Sul")) 

names(escolaridade2) <- c("Localizacao", "Sem_instrução_e_fundamental_incompleto", "Fundamental_completo_e_médio_incompleto", "Médio_completo_e_superior_incompleto", "Superior_completo", "Não_determinado")

# modificando os dados para facilitar na hora do plot
escolaridade2 <- escolaridade2 %>%
  gather(Instrucao, Populacao, -Localizacao)

g3 <- ggplot(data = escolaridade2, aes(x = as.factor(Localizacao), y = Populacao/1000000, fill = as.factor(Instrucao))) +
  geom_bar(stat = "identity", position = 'dodge') +
  ggtitle("Grau de instrução por Região em 2010") +
  xlab("Localização") + 
  ylab("População(em milhões)") +
  labs(fill = "Nível de instrução") +
  scale_fill_manual(values = c("#FF8C00", "#4B0082", "#8B4513", "#556B2F", "#4682B4"),
                    labels = c("Fundamental Completo e Médio incompleto", 
                               "Médio completo e superior incompleto", 
                               "Não determinado",
                               "Sem instrução e fundamental incompleto",
                               "Supeior Completo"))

multiplot(g1, g2, g3)

zika3 <- zika %>%
  select(c(report_date, location, location_type, value)) %>%
  filter(location_type == "region") %>%
  group_by(report_date, location) %>%
  summarise(casos = sum(value))


ggplot(zika3, aes(x=report_date, y=casos, color=location)) +
  geom_line() + 
  ggtitle("Número de casos de Zika ao longo do tempo") +
  xlab("Tempo") + 
  ylab("Número de casos") +
  scale_x_date(date_labels = "%d %b")


# Mapa interativo das regiões
library(leaflet)

zika <- getLatLong(zika)
zika_map <- (zika %>%
               filter(location_type == 'state') %>% 
               group_by(location, LAT, LON) %>%
               summarise(casos = sum(value))
)

leaflet(data = zika_map) %>%
  addTiles() %>%
  addCircles(lat = zika_map$LAT, 
             lng = zika_map$LON,
             popup = ~as.character(zika_map$casos),
             fillColor = c('#c00000'), 
             color = c('#c00000'),
             label = zika_map$location,
             radius = sapply(zika_map$casos, 
                             function(x){
                               if(x <= 10000 && x >= 3000)
                                 return(x*10)
                               else if(x>1000 && x < 3000)
                                 return(x*12)
                               else if(x <= 1000)
                                 return(x*18)
                               else
                                 return(x)
                             })) 


# função retirada do http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


getLatLong <- function(x, specialSymbol = '_', column = "location"){
  for(indice in 1:dim(x)[1])
  {
    if( tolower(unique(str_replace_all(x[indice, column], '_', ''))) == "acre"){ 
      x[indice, 'LAT'] <- -8.77
      x[indice, 'LON'] <- -70.55
    }
    else if( tolower(unique(str_replace_all(x[indice, column], '_', ''))) == "alagoas"){ 
      x[indice, 'LAT'] <- -9.62
      x[indice, 'LON'] <- -36.82
    }
    else if( tolower(unique(str_replace_all(x[indice, column], '_', ''))) == "amazonas"){ 
      x[indice, 'LAT'] <- -3.47
      x[indice, 'LON'] <- -65.10
    }
    else if( tolower(unique(str_replace_all(x[indice, column], '_', ''))) == "amapa"){ 
      x[indice, 'LAT'] <- -1.41
      x[indice, 'LON'] <- -51.77
    }
    else if( tolower(unique(str_replace_all(x[indice, column], '_', ''))) == "bahia"){ 
      x[indice, 'LAT'] <- -13.29
      x[indice, 'LON'] <-  -41.71
    }
    else if( tolower(unique(str_replace_all(x[indice, column], '_', ''))) == "ceara"){ 
      x[indice, 'LAT'] <- -5.20
      x[indice, 'LON'] <- -39.53
    }
    else if( tolower(unique(str_replace_all(x[indice, column], '_', ''))) == "distritofederal"){ 
      x[indice, 'LAT'] <- -15.83
      x[indice, 'LON'] <-  -47.86
    }
    else if( tolower(unique(str_replace_all(x[indice, column], '_', ''))) == "espiritosanto"){ 
      x[indice, 'LAT'] <- -19.19
      x[indice, 'LON'] <-  -40.34
    }
    else if( tolower(unique(str_replace_all(x[indice, column], '_', ''))) == "goias"){ 
      x[indice, 'LAT'] <- -15.98
      x[indice, 'LON'] <-  -49.86
    }
    else if( tolower(unique(str_replace_all(x[indice, column], '_', ''))) == "maranhao"){ 
      x[indice, 'LAT'] <- -5.42
      x[indice, 'LON'] <- -45.44
    }
    else if( tolower(unique(str_replace_all(x[indice, column], '_', ''))) == "matogrosso"){ 
      x[indice, 'LAT'] <- -12.64
      x[indice, 'LON'] <-  -55.42
    }
    else if( tolower(unique(str_replace_all(x[indice, column], '_', ''))) == "matogrossodosul"){ 
      x[indice, 'LAT'] <- -20.51
      x[indice, 'LON'] <-  -54.5
    }
    else if( tolower(unique(str_replace_all(x[indice, column], '_', ''))) == "minasgerais"){ 
      x[indice, 'LAT'] <- -18.10
      x[indice, 'LON'] <-  -44.38
    }
    else if( tolower(unique(str_replace_all(x[indice, column], '_', ''))) == "para"){ 
      x[indice, 'LAT'] <- -3.79
      x[indice, 'LON'] <- -52.48
    }
    else if( tolower(unique(str_replace_all(x[indice, column], '_', ''))) == "paraiba"){ 
      x[indice, 'LAT'] <- -7.28
      x[indice, 'LON'] <- -36.72
    }
    else if( tolower(unique(str_replace_all(x[indice, column], '_', ''))) == "parana"){ 
      x[indice, 'LAT'] <- -24.8
      x[indice, 'LON'] <-  -51.5
    }
    else if( tolower(unique(str_replace_all(x[indice, column], '_', ''))) == "pernambuco"){ 
      x[indice, 'LAT'] <- -8.38
      x[indice, 'LON'] <- -37.86
    }
    else if( tolower(unique(str_replace_all(x[indice, column], '_', ''))) == "piaui"){ 
      x[indice, 'LAT'] <- -6.60
      x[indice, 'LON'] <- -42.28
    }
    else if( tolower(unique(str_replace_all(x[indice, column], '_', ''))) == "riodejaneiro"){ 
      x[indice, 'LAT'] <- -22.2
      x[indice, 'LON'] <-  -42.6
    }
    else if( tolower(unique(str_replace_all(x[indice, column], '_', ''))) == "riograndedonorte"){ 
      x[indice, 'LAT'] <- -5.81
      x[indice, 'LON'] <- -36.59
    }
    else if( tolower(unique(str_replace_all(x[indice, column], '_', ''))) == "rondonia"){ 
      x[indice, 'LAT'] <- -10.83
      x[indice, 'LON'] <-  -63.34
    }
    else if( tolower(unique(str_replace_all(x[indice, column], '_', ''))) == "riograndedosul"){ 
      x[indice, 'LAT'] <- -30.17
      x[indice, 'LON'] <-  -53.50
    }
    else if( tolower(unique(str_replace_all(x[indice, column], '_', ''))) == "roraima"){ 
      x[indice, 'LAT'] <- -1.99
      x[indice, 'LON'] <- -61.33
    }
    else if( tolower(unique(str_replace_all(x[indice, column], '_', ''))) == "santacatarina"){ 
      x[indice, 'LAT'] <- -27.45
      x[indice, 'LON'] <-  -50.95
    }
    else if( tolower(unique(str_replace_all(x[indice, column], '_', ''))) == "sergipe"){ 
      x[indice, 'LAT'] <- -10.57
      x[indice, 'LON'] <-  -37.45
    }
    else if( tolower(unique(str_replace_all(x[indice, column], '_', ''))) == "saopaulo"){ 
      x[indice, 'LAT'] <- -22.19
      x[indice, 'LON'] <-  -48.79
    }
    else if( tolower(unique(str_replace_all(x[indice, column], '_', ''))) == "tocantins"){ 
      x[indice, 'LAT'] <- -9.46
      x[indice, 'LON'] <- -48.26
    }
    else{
      x[indice, 'LAT'] <- NA
      x[indice, 'LON'] <- NA
    }
  }
  return(x)
}
