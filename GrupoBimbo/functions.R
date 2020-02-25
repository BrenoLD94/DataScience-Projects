# ================================== FUNCOES ==============================================
# Obtenho as regiões baseado nos estados do méxico
getRegiao <- function(x){
  x = str_to_lower(x)
  x = str_replace_all(x, " ", "")
  
  noroeste <- c("bajacalifornianorte", "bajacaliforniasur", "chihuahua", "durango", "sinaloa", "sonora") 
  nordeste <- c("coahuila", "nuevoleón", "tamaulipas", "zacatecas")
  centroNorte <- c("aguascalientes", "guanajuato", "queretaro", "sanluispotosí")
  centroSul <- c("estadodeméxico", "méxico,d.f.", "morelos")
  oeste <- c("colima", "jalisco", "michoacán", "nayarit")
  leste <- c("hidalgo", "puebla", "tlaxcala", "veracruz")
  sudeste <- c("campeche", "quintanaroo", "tabasco", "yucatán")
  sudoeste <- c("chiapas", "guerrero", "oaxaca")
  
  # regioes - alguns nomes do dataframe estao diferente dos oficiais 
  if(x %in% noroeste)
    return("Noroeste")
  else if(x %in% nordeste)
    return("Nordeste")
  else if(x %in% centroNorte)
    return("CentroNorte")
  else if(x %in% centroSul)
    return("CentroSul")
  else if(x %in% oeste)
    return("Oeste")
  else if(x %in% leste)
    return("Leste")
  else if(x %in% sudeste)
    return("Sudeste")
  else if(x %in% sudoeste)
    return("Sudoeste")
  else
    return(NA)
}

# FUNÇÃO USADA PARA CRIAR UMA AMOSTRA
getAmostra <- function(path, SampleSize, name = "amostra.csv", pathToSave = "../Dados/"){
  temp <- fread(paste(pathToSave, path, sep = ''))
  
  if(SampleSize > nrow(temp)){
    print("Tamanho da amostra invalido")
    return(NULL)
  }
  
  set.seed(101)
  
  indices <- sample(1:nrow(temp), SampleSize)
  
  amostra <- temp[indices]
  
  write.csv(amostra, paste(pathToSave, name, sep = ''), row.names = TRUE)
  
  return(amostra)
}

# padroniza os dados
Scale <- function(x){
  if(!is.numeric(x))
    stop(paste(as.character(x), "nao é uma variável numérica"))
  
  dadospadronizados <- (x - mean(x))/sd(x)
  
  return(dadospadronizados)
}

# retorna os dados padronizados para o formato nao padronizado 
ReScale <- function(x_padronizado, mean, sd){
  if(!is.numeric(x_padronizado))
    stop(paste(as.character(x_padronizado), "nao é uma variável numérica"))
  
  x <- (x_padronizado*sd) + mean
  
  return(x)
}

# retorna os dados normalizados
normalization <- function(x){
  return((x - min(x))/(max(x) - min(x)))
}
